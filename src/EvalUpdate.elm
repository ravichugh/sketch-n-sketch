module EvalUpdate exposing (..)
{-
Everything from LangTools that depended on the prelude is in this file now.
Eval.run has become EvalUpdate.run, because it depends on the prelude and on the Update module.
-}
import Update
import UpdateStack exposing (UpdateStack, updateContext, UpdatedExp)
import UpdatedEnv exposing (UpdatedEnv)
import UpdateUtils exposing (defaultVDiffs, vDiffsToVal, valToVDiffs, recursiveMergeVal, diffsAt, toTupleDiffs)
import Eval
import Lang exposing (..)
import HTMLValParser
import LangParserUtils
import LangUtils exposing (..)
import Utils exposing (reverseInsert)
import Syntax exposing (Syntax)
import ElmParser as Parser
import Results exposing (Results, ok1)
import LazyList exposing (LazyList)
import LangTools exposing (..)
import ImpureGoodies
import Set exposing (Set)
import Dict exposing (Dict)
import ValUnparser exposing (strVal_, strOp, strLoc)
import ParserUtils
import ValBuilder as Vb
import ValUnbuilder as Vu
import UpdateRegex

builtinEnv =
  [ ("error", builtinVal "EvalUpdate.error" <| VFun "error" ["msg"] (oneArg "error" <| \arg ->
      case arg.v_ of
        VBase (VString s) -> Err s
        _ -> Err <| valToString arg
    ) Nothing)
  , ("parseHTML", HTMLValParser.htmlValParser)
   -- TODO: This && evaluates both sides, can we have something that does less?
  , ("&&", builtinVal "EvalUpdate.&&" <| VFun "&&" ["left", "right"] (twoArgs "&&" <| \left right ->
         case left.v_ of
           VBase (VBool True) -> Ok (right, [])
           VBase (VBool False) -> Ok (left, [])
           _ -> Err <| "&& expects two booleans, got " ++ valToString left
     ) Nothing)
  , ("||", builtinVal "EvalUpdate.||" <| VFun "||" ["left", "right"] (twoArgs "||" <| \left right ->
         case left.v_ of
           VBase (VBool True) -> Ok (left, [])
           VBase (VBool False) -> Ok (right, [])
           _ -> Err <| "|| expects two booleans, got " ++ valToString left
     ) Nothing)
  , ("<=", builtinVal "EvalUpdate.<=" <| VFun "<=" ["left", "right"] (twoArgs "<=" <| \left right ->
         case (left.v_, right.v_) of
           (VConst _ (n1, _), VConst _ (n2, _))  -> Ok (replaceV_ left <| VBase (VBool (n1 <= n2)), [])
           _ -> Err <| "<= expects two numbers, got " ++ valToString left ++ " and " ++ valToString right
     ) Nothing)
  , (">=", builtinVal "EvalUpdate.>=" <| VFun ">=" ["left", "right"] (twoArgs ">=" <| \left right ->
         case (left.v_, right.v_) of
           (VConst _ (n1, _), VConst _ (n2, _))  -> Ok (replaceV_ left <| VBase (VBool (n1 >= n2)), [])
           _ -> Err <| ">= expects two numbers, got " ++ valToString left ++ " and " ++ valToString right
     ) Nothing)
  , (">", builtinVal "EvalUpdate.>" <| VFun ">" ["left", "right"] (twoArgs ">" <| \left right ->
         case (left.v_, right.v_) of
           (VConst _ (n1, _), VConst _ (n2, _))  -> Ok (replaceV_ left <| VBase (VBool (n1 > n2)), [])
           _ -> Err <| "> expects two numbers, got " ++ valToString left ++ " and " ++ valToString right
     ) Nothing)
  , ("/=", builtinVal "EvalUpdate./=" <| VFun "/=" ["left", "right"] (twoArgs "/=" <| \left right ->
         case (left.v_, right.v_) of
           (VConst _ (n1, _), VConst _ (n2, _))  -> Ok (replaceV_ left <| VBase (VBool (n1 /= n2)), [])
           (_, _) -> Ok (replaceV_ left <| VBase <| VBool <| valToString left /= valToString right, [])
     ) Nothing)
  , ("getCurrentTime", builtinVal "EvalUpdate.getCurrentTime" (VFun "getCurrentTime" ["unit"] (\_ ->
      let n = ImpureGoodies.getCurrentTime () in
      let v_ = VConst Nothing (n, dummyTrace) in
      Ok (builtinVal "EvalUpdate.getCurrentTime.RESULT" v_, [])
    ) Nothing))
  , ("toggleGlobalBool", builtinVal "EvalUpdate.toggleGlobalBool" (VFun "toggleGlobalBool" ["unit"] (\_ ->
      let v = VBase (VBool (ImpureGoodies.toggleGlobalBool ())) in
      Ok (builtinVal "EvalUpdate.toggleGlobalBool.RESULT" v, [])
    ) Nothing))
  , ("%", builtinVal "EvalUpdate.%" <| VFun "%" ["left", "right"] (twoArgs "%" <| \left right ->
         case (left.v_, right.v_) of
           (VConst x (n1, y), VConst _ (n2, _))  ->
             if n2 == 0 then
               Err "Modulo by zero"
             else
               Ok (replaceV_ left <| VConst x (toFloat (truncate n1 % truncate n2), y), [])
           _ -> Err <| "% expects two numbers, got " ++ valToString left ++ " and " ++ valToString right
     ) Nothing)
  , (">>", builtinVal "EvalUpdate.>>" <| VFun ">>" ["left", "right", "x"] (\args ->
      case args of
        [left, right, x] ->
           let env = [("x", x), ("left", left), ("right", right)] in
           Eval.doEval Syntax.Elm env (eApp (eVar "right") [eApp (eVar "left") [eVar "x"]]) |> Result.map Tuple.first
        _ -> Err <| ">> expects 3 arguments, got " ++ toString (List.length args)
      ) (Just (\args oldVal newVal d -> case args of
      [left, right, x] ->
        let env = [("left", left), ("right", right), ("x", x)] in
        updateContext ">>" env (eApp (eVar "right") [eApp (eVar "left") [eVar "x"]]) [] oldVal newVal d |>
        update |>
          Results.filter (\(newEnv, newExp) -> newExp.changes == Nothing) |>
          Results.map (\(newEnv, _) ->
            case newEnv.val of
              [(_, newLeft), (_, newRight), (_, newX)] ->
                ([newLeft, newRight, newX], newEnv.changes)
              _ -> Debug.crash "[internal error] >> Environment is empty !!!"
            )
      _ -> Err <| ">> expects 3 arguments, got " ++ toString (List.length args)
    )))
  , ("<<", builtinVal "EvalUpdate.<<" <| VFun "<<" ["left", "right", "x"] (\args ->
    case args of
      [left, right, x] ->
         let env = [("x", x), ("left", left), ("right", right)] in
         Eval.doEval Syntax.Elm env (eApp (eVar "left") [eApp (eVar "right") [eVar "x"]]) |> Result.map Tuple.first
      _ -> Err <| ">> expects 2 arguments, got " ++ toString (List.length args)
    ) (Just (\args oldVal newVal d -> case args of
    [left, right, x] ->
      let env = [("left", left), ("right", right), ("x", x)] in
      updateContext ">>" env (eApp (eVar "left") [eApp (eVar "right") [eVar "x"]]) [] oldVal newVal d |>
      update |>
        Results.filter (\(newEnv, newExp) -> newExp.changes == Nothing) |>
        Results.map (\(newEnv, _) ->
          case newEnv.val of
            [(_, newLeft), (_, newRight), (_, newX)] -> ([newLeft, newRight, newX], newEnv.changes)
            _ -> Debug.crash "[internal error] << Environment is empty !!!"
          )
    _ -> Err <| "<< expects 3 arguments, got " ++ toString (List.length args)
  )))
  , ("__jsEval__", builtinVal "EvalUpdate.__jsEval__" <| VFun "__jsEval__" ["jsprogram"] (oneArg "__jsEval__" <| \program ->
     case Vu.string program of
       Ok s ->
         Ok (nativeToVal (Vb.fromVal program) <| ImpureGoodies.evaluate s, [])
       Err s-> Err <| "__jsEval__ expects a javascript program as a string, got " ++ LangUtils.valToString program
    ) Nothing)
  , ("__evaluate__", builtinVal "EvalUpdate.__evaluate__" <| VFun "__evaluate__" ["environment", "program"] (twoArgs "__evaluate__" <| \penv program ->
      case (Vu.list (Vu.tuple2 Vu.string Vu.identity) penv, program.v_) of
          (Ok env, VBase (VString s)) ->
              Syntax.parser Syntax.Elm s
              |> Result.mapError (ParserUtils.showError)
              |> Result.andThen (\prog ->
                  Eval.doEval Syntax.Elm (env ++ builtinEnv) prog
                )
              |> Result.map (Tuple.first >> Tuple.first)
              |> Vb.result Vb.identity (Vb.fromVal program)
              |> (\x -> Ok (x, []))
          _ -> Err <| "evaluate expects a List (String, values) and a program as a string, got " ++ LangUtils.valToString penv ++ " and " ++ LangUtils.valToString program
    ) <| Just <| twoArgsUpdate "__evaluate__" <| \oldpEnv oldProgram oldValr newValr d ->
          case (Vu.list (Vu.tuple2 Vu.string Vu.identity) oldpEnv, oldProgram.v_) of
            (Ok env, VBase (VString s)) ->
              let parsed = Syntax.parser Syntax.Elm s in
              case parsed of
                 Err err -> let (error, reverser) = ParserUtils.showErrorReversible err in
                  case Vu.result Vu.identity newValr of
                     Ok (Err msg2) ->
                        case reverser msg2 of
                           Just newProgram ->
                             let newProgramV = Vb.string (Vb.fromVal oldProgram) newProgram in
                             let newProgramDiffs = defaultVDiffs oldProgram newProgramV in
                             flip  Results.map  newProgramDiffs <| \pd ->
                               ([oldpEnv, newProgramV], Maybe.map (\d -> [(1, d)]) pd |> Maybe.withDefault [])
                           Nothing -> Err <| "No way to change the outpur of __evaluate__ from error to " ++ msg2 ++ "'"
                     Err msg -> Err msg
                     Ok (Ok x) -> Err <| "Cannot change the outpur of __evaluate__ from error to " ++ valToString x ++ "'"
                 Ok x -> ok1 x |> Results.andThen (\prog ->
                    case (Vu.result Vu.identity oldValr, Vu.result Vu.identity newValr, vDatatypeDiffsGet "_1" d) of
                      (Ok (Ok oldVal), Ok (Ok newVal), Just dd) ->
                        -- update = UpdateStack -> LazyList NextAction -> Results (UpdatedEnv, UpdatedExp)
                          UpdateStack.updateContext "Eval.__evaluate__" (env ++ builtinEnv) prog [] oldVal newVal dd
                         |> update
                         |> Results.filter (\(newEnv, newProg) ->
                             let envLength = List.length env in
                             newEnv.changes |> Utils.findFirst (\(i, _) -> i >= envLength) |> Utils.maybeIsEmpty
                           )
                         |> Results.andThen (\(newEnv, newProg) ->
                           let _ = Debug.log "#1" () in
                           let x = Syntax.unparser Syntax.Elm newProg.val in
                           let newProgram = replaceV_ oldProgram <| VBase <| VString x in
                           let newEnvValue = newEnv.val |> Vb.list (Vb.tuple2 Vb.string Vb.identity) (Vb.fromVal oldpEnv) in
                           let newEnvDiffs = newEnv.changes
                             |> List.map (\(i, d) -> (i, ListElemUpdate <| VRecordDiffs <| Dict.fromList  [("_2", d)]))
                             |>(\d -> [(0,  VListDiffs d)]) in
                           UpdateUtils.defaultVDiffs oldProgram newProgram |> Results.map (\mbd -> case mbd of
                             Nothing -> ([newEnvValue, newProgram], newEnvDiffs )
                             Just d -> ([newEnvValue, newProgram], newEnvDiffs ++ [(1, d)])
                           )
                         )
                      (_, _, Nothing) -> Err <| "Expected VRecordDiffs with 1 element, got " ++ toString d
                      (Err msg, _, _) -> Err msg
                      (_, Err msg, _) -> Err msg
                      (Ok (Err msg), Ok (Err msg2), _) -> if msg == msg2 then ok1 ([oldpEnv, oldProgram], []) else Err <| "Cannot change the error message of __evaluate__ to " ++ msg2
                      (Ok (Err msg), Ok (Ok x), _) -> Err <| "Cannot change the outpur of __evaluate__ from error '"++msg++"' to " ++ valToString x ++ "'"
                      (_, Ok (Err msg2), _) -> Err <| "Don't know how to update the result of a correct __evaluate__ by an error '" ++ msg2 ++ "'"
                    )
            _ -> Err <| "evaluate expects a List (String, values) and a program as a string, got " ++ LangUtils.valToString oldpEnv ++ " and " ++ LangUtils.valToString oldProgram
    )
  , ("__updateApp__", builtinVal "EvalUpdate.updateApp" <|
  VFun "__updateApp__" ["{fun,input[,oldOutput],output[,outputDiff]}"] (oneArg "__updateApp__" <| \arg ->
        let vb = Vb.fromVal arg in
        case arg.v_ of
          VRecord d ->
            case (Utils.dictGetFirst ["fun", "function"] d,
                  Utils.dictGetFirst ["input", "oldInput", "inputOld"] d,
                  Utils.dictGetFirst ["output", "newOutput", "outputNew"] d) of
              (Just fun, Just input, Just newVal) ->
                let xyEnv = [("x", fun),("y", input)] in
                let xyExp = (withDummyExpInfo <| EApp space0 (eVar "x") [eVar "y"] SpaceApp space0) in
                let oldOut = case Utils.dictGetFirst ["oldOutput", "oldOut", "outputOld"] d of
                  Nothing -> Eval.doEval Syntax.Elm xyEnv xyExp |> Result.map (\((v, _), _) -> v)
                  Just v -> Ok v
                in
                case oldOut of
                  Err msg -> Err <| "while evaluating updateApp and trying to compute the old value, " ++ msg
                  Ok oldOut ->
                    let outputDiff = case Utils.dictGetFirst ["outputDiff",  "diffOutput", "diffOut", "outDiff", "diffs"] d of
                      Nothing -> UpdateUtils.defaultVDiffs oldOut newVal |> Results.firstResult
                      Just v -> valToVDiffs v |> Result.map Just
                    in
                    --let _ = Debug.log "calling back update" () in
                    case outputDiff of
                      Err msg -> Err <| "while evaluating updateApp and trying to compute the output diff, " ++ msg
                      Ok Nothing -> -- No need to call update
                        let resultingValue = Vb.record (Vb.list Vb.identity) vb (
                             Dict.fromList [("values", [input]),
                                            ("diffs", [Vb.maybe vDiffsToVal vb Nothing] )
                             ])
                        in
                        Ok (resultingValue, [])
                      Ok (Just newOutDiffs) ->
                        let basicResult = case update <| updateContext "__updateApp__" xyEnv xyExp [] oldOut newVal newOutDiffs of
                          Err msg -> Vb.record Vb.string vb (Dict.fromList [("error", msg)])
                          Ok ll ->
                             let l = LazyList.toList ll in
                             let lFiltered = List.filter (\(newXYEnv, newExp) ->
                               case newXYEnv.changes of
                                  [] -> True
                                  [(1, _)] -> True
                                  _ -> False) l
                             in
                             if List.isEmpty lFiltered then
                               if List.isEmpty l then
                                 Vb.record (Vb.list Vb.identity) vb (Dict.fromList [("values", []), ("diffs", [])])
                               else
                                 Vb.record Vb.string vb (Dict.fromList [("error", "Only solutions modifying the constant function of __updateApp__")])
                             else
                               let (results, diffs) = lFiltered |> List.map (\(newXYEnv, newExp) ->
                                 case newXYEnv.val of
                                    [("x", newFun), ("y",newArg)] ->
                                      case newXYEnv.changes of
                                        [] -> (newArg, Nothing)
                                        [(1, diff)] -> (newArg, Just diff)
                                        _ -> Debug.crash "Internal error: expected not much than (1, diff) in environment changes"
                                    _ -> Debug.crash "Internal error: expected x and y in environment"
                                 ) |> List.unzip in
                               let maybeDiffsVal = diffs |> Vb.list (Vb.maybe vDiffsToVal) vb in
                               Vb.record Vb.identity vb (
                                    Dict.fromList [("values", Vb.list Vb.identity vb results),
                                                 ("diffs", maybeDiffsVal )
                                    ])
                        in Ok (basicResult, [])
              (mbFun, mbInput, mbOutput) ->
                Err <|
                  "__updateApp__ requires a record with at least {fun,input,output}. Missing" ++
                 (Maybe.map (\_ -> "") mbFun |> Maybe.withDefault " fun") ++
                 (Maybe.map (\_ -> "") mbInput |> Maybe.withDefault " input") ++
                 (Maybe.map (\_ -> "") mbOutput |> Maybe.withDefault " output")
          _ -> Err <| "__updateApp__ argument should be a record {fun,input[,oldOutput],output[,outputDiff]}, but got " ++ valToString arg
  ) Nothing)
  , ("__merge__",  builtinVal "EvalUpdate.merge" <|
     VFun "__merge__" ["original", "List (modified, Maybe diff)"] (twoArgs "__merge__" <| \original modifications ->
       case Vu.list (Vu.tuple2 Vu.identity (Vu.maybe valToVDiffs)) modifications of
         Ok withModifs ->
           let (newVal, mbd) = recursiveMergeVal original withModifs in
           Ok ((Vb.tuple2 Vb.identity (Vb.maybe vDiffsToVal)) (Vb.fromVal original) (newVal, mbd), [])
         Err msg -> Err  <| "__merge__ 's second argument should be a list of (value, Maybe diff), but got " ++ valToString modifications ++ "\n" ++ msg
     ) Nothing)
  , ("__diff__", builtinVal "EvalUpdate.diff" <|
    VFun "__diff__" ["value_before", "value_after"] (twoArgs "__diff__" <| \before after ->
          Ok ( defaultVDiffs before after |> Results.firstResult
                 |> Vb.result (Vb.maybe vDiffsToVal) (Vb.fromVal before)
               , [])
    ) Nothing)
  , ("replaceAllIn", UpdateRegex.replaceAllByIn eval update)
  , ("replaceFirstIn", UpdateRegex.replaceFirstByIn eval update)
  , ("updateReplace", UpdateRegex.updateReplace eval update)
  , ("findInterleavings", UpdateRegex.findInterleavings update)
  , ("join__", UpdateRegex.join)

  , ("__mbwraphtmlnode__", builtinVal "EvalUpdate.__mbwraphtmlnode__" <|
     VFun "__mbwraphtmlnode__" ["string_node_listnode"] (oneArg "string_node_listnode" <| \original ->
       case original.v_ of
         VConst _ (content, _) ->
           Ok (Vb.list (Vb.viewtuple2 Vb.string Vb.string) (Vb.fromVal original) [("TEXT", toString content)], [])
         VBase (VString content) ->
           Ok (Vb.list (Vb.viewtuple2 Vb.string Vb.string) (Vb.fromVal original) [("TEXT", content)], [])
         VList (head::tail) ->
           case head.v_ of
             VBase (VString tagName) ->
               Ok (Vb.list Vb.identity (Vb.fromVal original) [original], [])
             _ -> Ok (original, [])
         _ -> Ok (original, [])
     ) <| Just <| oneArgUpdate "string_node_listnode" <| \original oldVal newVal diffs ->
      let unwrapExpDiffs newVal diffs=
         case (newVal.v_, diffs) of
           (VList [elem], VListDiffs [(0, ListElemUpdate d)]) ->
             --let _ = Debug.log ("elem, d: " ++ valToString elem ++ ", " ++ toString d) () in
             case (Vu.viewtuple2 Vu.string Vu.string elem, d) of
               (Ok ("TEXT", newContent), VListDiffs [(1, ListElemUpdate (ds))]) ->
                 Ok (Just (Just (newContent, ds)))
               (_, VListDiffs []) -> Ok (Just Nothing)
               (Err msg, _) -> Err msg
               (_, _) -> Ok Nothing
           (_, VListDiffs []) -> Ok (Just Nothing)
           _ -> Ok Nothing
      in
      case original.v_ of
        VConst _ (content, _) -> -- We make sure no element was inserted and we unwrap the value and the diffs
          case unwrapExpDiffs newVal diffs of
            Err msg -> Err msg
            Ok Nothing -> Ok LazyList.Nil
            Ok (Just Nothing) -> ok1 ([original], [])
            Ok (Just (Just (newContent, ds))) ->
              case String.toFloat newContent of
                Err msg -> Ok LazyList.Nil
                Ok newFloat -> ok1 ([Vb.const (Vb.fromVal original) newFloat], [(0, VConstDiffs)])
        VBase (VString content) -> -- We make sure no element was inserted and we unwrap the value and the diffs
          case unwrapExpDiffs newVal diffs of
            Err msg -> Err msg
            Ok Nothing -> Ok LazyList.Nil
            Ok (Just Nothing) -> ok1 ([original], [])
            Ok (Just (Just (newContent, ds))) ->
              ok1 ([Vb.string (Vb.fromVal original) newContent], [(0, ds)])
        VList (head::tail) ->
          case (head.v_, newVal.v_, diffs) of
            (VBase (VString tagName), VList [newElem], VListDiffs [(0, ListElemUpdate d)]) ->
              ok1 ([newElem], [(0, d)])
            _ -> ok1 ([newVal], [(0, diffs)])
        _ -> ok1 ([newVal], [(0, diffs)])
    )
  {-, ("toPathData", builtinVal "EvalUpdate.toPathData" <|
    VFun "toPathData" ["d_str"] (oneArg "toPathData" <| \original ->
      case original.v_ of
        VList _ -> Ok (original, [])
        VBase (VString content) ->
          Regex.split (Regex.regex "")

    ) <| Just <| oneArgUpdate "__mbpathdsplit__" <| \original oldVal newVal diffs ->
      case original.v_ of
        VList _ -> Ok (original, [])
        VBase (VString content) ->
  )-}
  , ("__mbstylesplit__", builtinVal "EvalUpdate.__mbstylesplit__" <|
     VFun "__mbstylesplit__" ["style_str"] (oneArg "__mbstylesplit__" <| \original ->
       case original.v_ of
         VList _ -> Ok (original, [])
         VBase (VString content) ->
           let vb = Vb.fromVal original in
           let finalVal =
             LangParserUtils.explodeStyleValue content |> List.map (\(_, name, _, value, _) ->
              (name, value)
             ) |>
             Vb.list (Vb.viewtuple2 Vb.string Vb.string) vb
           in
           Ok (finalVal, [])
         _ -> Err <| "__mbstylesplit__ takes a string or a list, got " ++ valToString original
     ) <| Just <| oneArgUpdate "__mbstylesplit__" <| \original oldVal newVal diffs ->
          case original.v_ of
            VList _ -> ok1 ([newVal], UpdateUtils.combineTupleDiffs [(0, Just diffs)] |> Maybe.withDefault [])
            VBase (VString content) -> -- Need to transform the List diffs into string diffs
              let originalStyles = LangParserUtils.explodeStyleValue content in
              case (diffs, Vu.list (Vu.viewtuple2 Vu.string Vu.string) newVal) of
                (VListDiffs diffElems, Ok updatedStyles) ->
                  let combineOldString (s1, s2, s3, s4, s5) = s1 ++ s2 ++ s3 ++ s4 ++ s5 in
                  let aux:Int ->
                            List (String, String, String, String, String) ->
                                           List (String, String) ->
                                                         ListDiffs VDiffs ->
                                                                   (String,    Int,    List StringDiffs) -> Results String (List Val, TupleDiffs VDiffs)
                      aux i originalStyles updatedStyles diffElems (accString, originalOffset, revAccDiffs) = case diffElems of
                    [] ->
                       let finalDiffs = Debug.log "finalDiffs" <| (UpdateUtils.combineTupleDiffs [(0, Just <| VStringDiffs (List.reverse revAccDiffs))] |> Maybe.withDefault []) in
                       let remainingAcc = List.map combineOldString originalStyles |> String.join ";" in
                       let finalString = replaceV_ original <| VBase <| VString (Debug.log "final string:"  (accString ++ remainingAcc)) in
                       ok1 ([finalString], finalDiffs)
                    (j, d)::tailDiffElems ->
                       if j > i then
                        let (originalStylesKept, originalStylesTail) = Utils.split (j - i) originalStyles in
                        let newUpdatedStyles = List.drop (j - i) updatedStyles in
                        let newString = originalStylesKept |> List.map combineOldString |> String.join "" in
                        let newFinalString = accString ++ newString in
                        let newOffset = originalOffset + String.length newString in
                        aux j originalStylesTail newUpdatedStyles diffElems (newFinalString, newOffset, revAccDiffs)
                       else -- j == i
                        case d of
                          ListElemDelete count ->
                            let (originalStylesRemoved, originalStylesTail) = Utils.split count originalStyles in
                            let oldString = originalStylesRemoved |> List.map (\(s1, s2, s3, s4, s5) -> s1 ++ s2 ++ s3 ++ s4 ++ s5) |> String.join "" in
                            let newOriginalOffset = originalOffset + String.length (List.map combineOldString originalStylesRemoved |> String.join "") in
                            let deletionPoint = originalOffset in
                            (accString, newOriginalOffset, [StringUpdate deletionPoint  (deletionPoint + String.length oldString) 0]) |>
                            aux (j + 1) originalStylesTail updatedStyles tailDiffElems
                          ListElemInsert count ->
                            let (insertedStyles, tailUpdatedStyles) = Utils.split count updatedStyles in
                            let insertedString = (if String.endsWith ";" accString || accString == "" then "" else ";") ++
                              (List.map (\(name, value) -> name ++ ":" ++ value ++ ";") insertedStyles |> String.join "")
                            in
                            let insertionPoint = originalOffset + String.length accString in
                            let _ = Debug.log "inserted" (count, originalOffset, insertionPoint, insertedString) in
                            (accString ++ insertedString, originalOffset, [StringUpdate insertionPoint insertionPoint (String.length insertedString)]) |>
                            aux j originalStyles tailUpdatedStyles tailDiffElems
                          ListElemUpdate d ->
                            case (originalStyles, updatedStyles) of
                              ((oldPreName, oldName, oldColon, oldValue, oldPostValue) :: originalStylesTail,
                               (newName, newValue)::updatedStylesTail) ->
                                 case vListDiffsUnapply d |> Maybe.andThen toTupleDiffs of
                                   Nothing -> Err "[Cannot add an elemnt inside a style attribute definition]"
                                   Just tupleDiffs ->
                                      let nameDiffs = case diffsAt 0 tupleDiffs of
                                        Nothing -> []
                                        Just (VStringDiffs strDiffs) -> offsetStr (
                                           originalOffset + String.length oldPreName) strDiffs
                                        Just _ ->
                                            let basepoint = originalOffset + String.length oldPreName in
                                            [StringUpdate basepoint (basepoint  + String.length oldName) (String.length newName) ]
                                      in
                                      let valueDiffs = case diffsAt 1 tupleDiffs of
                                        Nothing -> []
                                        Just (VStringDiffs strDiffs) -> offsetStr (
                                           originalOffset + String.length oldPreName + String.length oldName + String.length oldColon) strDiffs
                                        Just _ ->
                                            let basepoint = originalOffset + String.length oldPreName + String.length oldName + String.length oldColon in
                                            [StringUpdate basepoint (basepoint  + String.length oldValue) (String.length newValue) ]
                                      in
                                      let newString = oldPreName ++ newName ++ oldColon ++ newValue ++ oldPostValue in
                                      let newOriginalOffset = originalOffset + String.length oldPreName + String.length oldName + String.length oldColon + String.length oldValue + String.length oldPostValue in
                                      (accString ++ newString, newOriginalOffset, revAccDiffs |> reverseInsert nameDiffs |> reverseInsert valueDiffs) |>
                                      aux (i + 1) originalStylesTail updatedStylesTail tailDiffElems
                              _ -> Err <| "[Internal error]: the diff is not consistent " ++ valToString oldVal ++ " " ++ valToString newVal ++ " " ++ toString diffs
                  in
                  aux 0 originalStyles updatedStyles diffElems ("", 0, [])
                (_, Err msg) -> Err <| "Expected VListDiffs got an error: " ++ msg
                _ -> Err <| "Expected VListDiffs and a List, got " ++ toString diffs ++ " and " ++ valToString newVal
            _ -> Err <| "__mbstylesplit__ takes a string or a List, got " ++ valToString newVal
    )
  ]

oneArg: String -> (Val -> Result String a) -> (List Val -> Result String a)
oneArg msg fun args = case args of
    [arg] -> fun arg
    _ -> Err <| msg ++ " takes 1 argument, got " ++ toString (List.length args)

twoArgs: String -> (Val -> Val -> Result String a) -> (List Val -> Result String a)
twoArgs msg fun args = case args of
    [left, right] -> fun left right
    _ -> Err <| msg ++ " takes 2 arguments, got " ++ toString (List.length args)

oneArgUpdate: String -> (Val -> a -> b -> c -> Results String e) -> (List Val -> a -> b -> c -> Results String e)
oneArgUpdate msg fun args a b c = case args of
    [arg] -> fun arg a b c
    _ -> Err <| msg ++ " takes 1 argument, got " ++ toString (List.length args)

twoArgsUpdate: String -> (Val -> Val -> a -> b -> c -> Results String e) -> (List Val -> a -> b -> c -> Results String e)
twoArgsUpdate msg fun args a b c = case args of
    [left, right] -> fun left right a b c
    _ -> Err <| msg ++ " takes 2 arguments, got " ++ toString (List.length args)

eval env e = Eval.doEval Syntax.Elm env e |> Result.map Tuple.first
update updateStack = Update.update LazyList.Nil LazyList.Nil updateStack

preludeEnvRes = Result.map Tuple.second <| (Eval.eval Syntax.Little builtinEnv [] Parser.prelude)
preludeEnv = Utils.fromOk "Eval.preludeEnv" <| preludeEnvRes

run : Syntax -> Exp -> Result String (Val, Widgets)
run syntax e =
-- doEval syntax initEnv e |> Result.map Tuple.first
  ImpureGoodies.logTimedRun "Eval.run" <| \() ->
    Eval.doEval syntax preludeEnv e |> Result.map Tuple.first

runWithEnv : Syntax -> Exp -> Result String ((Val, Widgets), Env)
runWithEnv syntax e =
-- doEval syntax initEnv e |> Result.map Tuple.first
  ImpureGoodies.logTimedRun "Eval.run" <| \() ->
    Eval.doEval syntax preludeEnv e

doUpdate : Exp -> Env -> Val -> Result String Val -> Results String (UpdatedEnv, UpdatedExp)
doUpdate oldExp oldEnv oldVal newValResult =
  newValResult
    --|> Result.map (\x -> let _ = Debug.log "#1" () in x)
    |> Results.fromResult
    |> Results.andThen (\newVal ->
      let thediffs = ImpureGoodies.logTimedRun "UpdateUtils.defaultVDiffs (doUpdate) " <| \_ -> UpdateUtils.defaultVDiffs oldVal newVal
      in
      case thediffs of
        Err msg -> Err msg
        Ok (LazyList.Nil ) -> Err "[Internal error] expected a diff or an error, got Nil"
        Ok (LazyList.Cons Nothing _ ) -> ok1 (UpdatedEnv.original preludeEnv, UpdatedExp oldExp Nothing)
        Ok ll ->
           ImpureGoodies.logTimedRun "Update.update (doUpdate) " <| \_ ->
            Ok (ll |> LazyList.filterMap identity) |> Results.andThen (\diffs ->
              let previousLets = UpdateStack.keepLets preludeEnv oldEnv in
              update <| updateContext "initial update" preludeEnv oldExp previousLets oldVal newVal diffs)
           )


doUpdateWithoutLog : Exp -> Env -> Val -> Val -> Results String (UpdatedEnv, UpdatedExp)
doUpdateWithoutLog oldExp oldEnv oldVal newVal =
  let thediffs = UpdateUtils.defaultVDiffs oldVal newVal
  in
  case thediffs of
    Err msg -> Err msg
    Ok (LazyList.Nil ) -> Err "[Internal error] expected a diff or an error, got Nil"
    Ok (LazyList.Cons Nothing _ ) -> ok1 (UpdatedEnv.original preludeEnv, UpdatedExp oldExp Nothing)
    Ok ll ->
        Ok (ll |> LazyList.filterMap identity) |> Results.andThen (\diffs ->
         --let _ = Debug.log ("update with diffs: " ++ UpdateUtils.vDiffsToString oldVal newVal diffs) () in
         let previousLets = UpdateStack.keepLets preludeEnv oldEnv in
         update <| updateContext "initial update" preludeEnv oldExp previousLets oldVal newVal diffs)

-- Deprecated
parseAndRun : String -> String
parseAndRun = valToString << Tuple.first << Utils.fromOk_ << run Syntax.Little << Utils.fromOkay "parseAndRun" << Parser.parse

parseAndRun_ = strVal_ True << Tuple.first << Utils.fromOk_ << run Syntax.Little << Utils.fromOkay "parseAndRun_" << Parser.parse

preludeIdentifiers = preludeEnv |> List.map Tuple.first |> Set.fromList

identifiersSetPlusPrelude : Exp -> Set.Set Ident
identifiersSetPlusPrelude exp =
  Set.union (identifiersSet exp) preludeIdentifiers


assignUniqueNames : Exp -> (Exp, Dict Ident Ident)
assignUniqueNames program =
  let initialUsedNames =
    -- Want to rename _everything_ so that there's multiple options for how to rename back to the originals
    identifiersSetPlusPrelude program
  in
  let (newProgram, usedNames, newNameToOldName) =
    assignUniqueNames_ program initialUsedNames Dict.empty
  in
  (newProgram, newNameToOldName)


assignUniqueNames_ : Exp -> Set.Set Ident -> Dict Ident Ident -> (Exp, Set.Set Ident, Dict Ident Ident)
assignUniqueNames_ exp usedNames oldNameToNewName =
  let recurse = assignUniqueNames_ in
  let recurseExps es =
    es
    |> List.foldl
        (\e (newEs, usedNames, newNameToOldName) ->
          let (newE, usedNames_, newNameToOldName_) = recurse e usedNames oldNameToNewName in
          (newEs ++ [newE], usedNames_, Dict.union newNameToOldName_ newNameToOldName)
        )
        ([], usedNames, Dict.empty)
  in
  let recurseExp e =
    let (newEs, usedNames, newNameToOldName) = recurseExps [e] in
    (Utils.head "assignUniqueNames_ head1" newEs, usedNames, newNameToOldName)
  in
  let assignUniqueNamesToPat_ pat usedNames =
    identifiersListInPat pat
    |> List.foldl
        (\name (pat, usedNames, oldNameToNewName) ->
          if Set.member name usedNames then
            let newName = nonCollidingName name 2 usedNames in
            (renameIdentifierInPat name newName pat, Set.insert newName usedNames, Dict.insert name newName oldNameToNewName)
          else
            (pat, Set.insert name usedNames, oldNameToNewName)
        )
        (pat, usedNames, Dict.empty)
  in
  let leafUnchanged = (exp, usedNames, Dict.empty) in
  case exp.val.e__ of
    EConst _ _ _ _  -> leafUnchanged
    EBase _ _       -> leafUnchanged
    EVar ws oldName ->
      case Dict.get oldName oldNameToNewName of
        Just newName  -> (replaceE__ exp (EVar ws newName), usedNames, Dict.empty)
        Nothing       -> leafUnchanged

    EFun ws1 ps e ws2 ->
      let (newPs, usedNames_, oldNameToNewNameAdditions) =
        ps
        |> List.foldl
            (\p (newPs, usedNames, oldNameToNewNameAdditions) ->
              let (newPat, usedNames_, oldNameToNewNameAdditions_) = assignUniqueNamesToPat_ p usedNames in
              (newPs ++ [newPat], usedNames_, Dict.union oldNameToNewNameAdditions_ oldNameToNewNameAdditions)
            )
            ([], usedNames, Dict.empty)
      in
      let (newBody, usedNames__, newNameToOldName) = recurse e usedNames_ (Dict.union oldNameToNewNameAdditions oldNameToNewName) in
      let newNameToOldName_ = Dict.union (Utils.flipDict oldNameToNewNameAdditions) newNameToOldName in
      ( replaceE__ exp (EFun ws1 newPs newBody ws2)
      , usedNames__
      , newNameToOldName_
      )

    EOp ws1 wso op es ws2 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps es in
      ( replaceE__ exp (EOp ws1 wso op newEs ws2)
      , usedNames_
      , newNameToOldName
      )

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps (Utils.listValues es) in
      ( replaceE__ exp (EList ws1 (Utils.listValuesMake es newEs) ws2 Nothing ws3)
      , usedNames_
      , newNameToOldName
      )

    EList ws1 es ws2 (Just tail) ws3 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps (Utils.listValues es ++ [tail]) in
      let (newHeads, newTail) = (Utils.removeLastElement newEs, Utils.last "assignUniqueNames_" newEs) in
      ( replaceE__ exp (EList ws1 (Utils.listValuesMake es newHeads) ws2 (Just newTail) ws3)
      , usedNames_
      , newNameToOldName
      )

    ERecord ws1 Nothing es ws2 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps (Utils.recordValues es) in
      ( replaceE__ exp (ERecord ws1 Nothing (Utils.recordValuesMake es newEs) ws2)
      , usedNames_
      , newNameToOldName
      )

    ERecord ws1 (Just (init, wsi)) es ws2 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps (Utils.recordValues es ++ [init]) in
      let (newFields, newInit) = (Utils.removeLastElement newEs, Utils.last "assignUniqueNames_" newEs) in
      ( replaceE__ exp (ERecord ws1 (Just (newInit, wsi)) (Utils.recordValuesMake es newFields) ws2)
      , usedNames_
      , newNameToOldName
      )

    ESelect ws0 e1 ws1 ws2 s ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
       ( replaceE__ exp (ESelect ws0 newE1 ws1 ws2 s)
       , usedNames_
       , newNameToOldName
       )

    EIf ws1 e1 ws2 e2 ws3 e3 ws4 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps [e1, e2, e3] in
      case newEs of
        [newE1, newE2, newE3] ->
          ( replaceE__ exp (EIf ws1 newE1 ws2 newE2 ws3 newE3 ws4)
          , usedNames_
          , newNameToOldName
          )

        _ ->
          Debug.crash "assignUniqueNames_ EIf"

    ECase ws1 e1 bs ws2 ->
      let (newScrutinee, usedNames_, newNameToOldName) = recurse e1 usedNames oldNameToNewName in
      let (newBranches, usedNames__, newNameToOldName_) =
        bs
        |> List.foldl
            (\branch (newBranches, usedNames, newNameToOldName) ->
              let (Branch_ bws1 bPat bExp bws2) = branch.val in
              let (newPat, usedNames_, oldNameToNewNameAdditions) = assignUniqueNamesToPat_ bPat usedNames in
              let (newBody, usedNames__, newNameToOldName_) = recurse bExp usedNames_ (Dict.union oldNameToNewNameAdditions oldNameToNewName) in
              let newBranch = { branch | val = Branch_ bws1 newPat newBody bws2 } in
              (newBranches ++ [newBranch], usedNames__, Dict.union (Utils.flipDict oldNameToNewNameAdditions) (Dict.union newNameToOldName_ newNameToOldName))
            )
            ([], usedNames_, newNameToOldName)
      in
      ( replaceE__ exp (ECase ws1 newScrutinee newBranches ws2)
      , usedNames__
      , newNameToOldName_
      )

    ETypeCase ws1 e1 tbs ws2 ->
      let (newScrutinee, usedNames_, newNameToOldName) = recurse e1 usedNames oldNameToNewName in
      let (newTBranches, usedNames__, newNameToOldName_) =
        tbs
        |> List.foldl
            (\tbranch (newTBranches, usedNames, newNameToOldName) ->
              let (TBranch_ bws1 bType bExp bws2) = tbranch.val in
              let (newBody, usedNames_, newNameToOldName_) = recurse bExp usedNames oldNameToNewName in
              let newTBranch = { tbranch | val = TBranch_ bws1 bType newBody bws2 } in
              (newTBranches ++ [newTBranch], usedNames_, Dict.union newNameToOldName_ newNameToOldName)
            )
            ([], usedNames_, newNameToOldName)
      in
      ( replaceE__ exp (ETypeCase ws1 newScrutinee newTBranches ws2)
      , usedNames__
      , newNameToOldName_
      )

    EApp ws1 e1 es appType ws2 ->
      let (newE1AndEs, usedNames_, newNameToOldName) = recurseExps (e1::es) in
      let (newE1, newEs) = (Utils.head "assignUniqueNames_ head" newE1AndEs, Utils.tail "assignUniqueNames_ tail" newE1AndEs) in
      ( replaceE__ exp (EApp ws1 newE1 newEs appType ws2)
      , usedNames_
      , newNameToOldName
      )

    ELet ws1 kind isRec p ws2 e1 ws3 e2 ws4 ->
      let (newPat, usedNames_, oldNameToNewNameAdditions) = assignUniqueNamesToPat_ p usedNames in
      let oldNameToNewNameWithAdditions = Dict.union oldNameToNewNameAdditions oldNameToNewName in
      let oldNameToNewNameForBoundExp =
        if isRec then
          oldNameToNewNameWithAdditions
        else
          oldNameToNewName
      in
      let (newE1, usedNames__, newNameToOldName)   = recurse e1 usedNames_ oldNameToNewNameForBoundExp in
      let (newE2, usedNames___, newNameToOldName_) = recurse e2 usedNames__ oldNameToNewNameWithAdditions in
      let newNameToOldName__ = Dict.union (Utils.flipDict oldNameToNewNameAdditions) (Dict.union newNameToOldName_ newNameToOldName) in
      ( replaceE__ exp (ELet ws1 kind isRec newPat ws2 newE1 ws3 newE2 ws4)
      , usedNames___
      , newNameToOldName__
      )

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (EOption ws1 s1 ws2 s2 newE1)
      , usedNames_
      , newNameToOldName
      )

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (ETyp ws1 pat tipe newE1 ws2)
      , usedNames_
      , newNameToOldName
      )

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (EColonType ws1 newE1 ws2 tipe ws3)
      , usedNames_
      , newNameToOldName
      )

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (ETypeAlias ws1 pat tipe newE1 ws2)
      , usedNames_
      , newNameToOldName
      )

    ETypeDef ws1 ident vars ws2 dcs e1 ws3 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (ETypeDef ws1 ident vars ws2 dcs newE1 ws3)
      , usedNames_
      , newNameToOldName
      )

    EParens ws1 e1 pStyle ws2 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (EParens ws1 newE1 pStyle ws2)
      , usedNames_
      , newNameToOldName
      )

    EHole _ _ -> leafUnchanged


-- What variable names are in use at any of the given locations?
-- For help finding unused names during synthesis.
visibleIdentifiersAtEIds : Exp -> Set.Set EId -> Set.Set Ident
visibleIdentifiersAtEIds program eids =
  let programIdents = visibleIdentifiersAtPredicateNoPrelude program (\exp -> Set.member exp.val.eid eids) in
  Set.union programIdents preludeIdentifiers


newVariableVisibleTo : EId -> Ident -> Int -> Exp -> List EId -> Exp -> (Ident, Exp)
newVariableVisibleTo insertedLetEId suggestedName startingNumberForNonCollidingName boundExp observerEIds program =
  let
    newName =
      nonCollidingName suggestedName startingNumberForNonCollidingName (visibleIdentifiersAtEIds program (Set.fromList observerEIds))
    eidToWrap =
      deepestCommonAncestorWithNewline program (\exp -> List.member exp.val.eid observerEIds) |> .val |> .eid
    newProgram =
      program
      |> mapExpNode
          eidToWrap
          (\expToWrap ->
            newLetFancyWhitespace insertedLetEId False (pVar newName) boundExp expToWrap program
          )
  in
  (newName, newProgram)


identifiersVisibleAtProgramEnd : Exp -> Set.Set Ident
identifiersVisibleAtProgramEnd program =
  let lastEId = (lastExp program).val.eid in
  visibleIdentifiersAtEIds program (Set.singleton lastEId)

-- External API

compileVal: Env -> String
compileVal e = Debug.crash "not implemented compile yet"

compileEnv: Env -> String
compileEnv e = Debug.crash "not implemented compile yet"

compile: Exp -> String
compile e = Debug.crash "not implemented compile yet"

parse: String -> Result String Exp
parse s =
  Syntax.parser Syntax.Elm s
  |> Result.mapError ParserUtils.showError

unparse: Exp -> String
unparse e = Syntax.unparser Syntax.Elm e

evalExp: Exp -> Result String Val
evalExp exp = run Syntax.Elm exp |> Result.map Tuple.first

updateExp: Exp -> Val -> Val -> Results String Exp
updateExp oldExp oldVal newVal =
  let thediffs = UpdateUtils.defaultVDiffs oldVal newVal
  in
  case thediffs of
    Err msg -> Err msg
    Ok (LazyList.Nil ) -> Err "[Internal error] expected a diff or an error, got Nil"
    Ok (LazyList.Cons Nothing _ ) -> ok1 oldExp
    Ok ll ->
        Ok (ll |> LazyList.filterMap identity) |> Results.andThen (\diffs ->
         --let _ = Debug.log ("update with diffs: " ++ UpdateUtils.vDiffsToString oldVal out diffs) () in
         (update <| updateContext "initial update" preludeEnv oldExp [] oldVal newVal diffs)) |>
         Results.map (\(newEnv, newExp) -> newExp.val)

valToNative: Val -> Result String a
valToNative v = case Vu.string v of
  Ok x -> Ok <| ImpureGoodies.hideType x
  Err msg -> case Vu.num v of
    Ok i -> Ok <| ImpureGoodies.hideType i
    Err msg -> case Vu.bool v of
      Ok b -> Ok <| ImpureGoodies.hideType b
      Err msg -> case Vu.list Vu.identity v of
        Ok l -> List.map valToNative l |> Utils.projOk |> Result.map ImpureGoodies.toNativeArray
        Err msg -> case Vu.record valToNative v of
          Ok d -> Ok <| ImpureGoodies.keyPairsToNativeRecord <| Dict.toList d
          Err msg -> Err <| "I only know how to convert vals to string, ints, booleans, list and records"


nativeToVal: Vb.Vb -> a -> Val
nativeToVal vb v =
  ImpureGoodies.fromNative v
    (\s -> Vb.string vb s)
    (\n -> Vb.num vb n)
    (\b -> Vb.bool vb b)
    (\l -> Vb.list nativeToVal vb l)
    (\r -> Vb.record nativeToVal vb (Dict.fromList r))

evaluate: String -> Result String Val
evaluate s =
  Syntax.parser Syntax.Elm s
  |> Result.mapError ParserUtils.showError
  |> Result.andThen (\exp ->
    run Syntax.Elm exp
  ) |> Result.map Tuple.first

valToString: Val -> String
valToString v = LangUtils.valToString v

api = {
  parse = parse,
  evalExp = evalExp,
  updateExp = updateExp,
  unparse = unparse,
  andThen = Result.andThen,
  valToNative = valToNative,
  nativeToVal = nativeToVal (builtinVal "EvalUpdate.nativeToVal"),

  toString = valToString,
  evaluate = evaluate
  }
