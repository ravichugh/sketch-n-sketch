module EvalUpdate exposing (..)
{-
Everything from LangTools that depended on the prelude is in this file now.
Eval.run has become EvalUpdate.run, because it depends on the prelude and on the Update module.
-}
import Update
import UpdateStack exposing (UpdateStack, updateContext)
import UpdatedEnv
import UpdateUtils exposing (defaultVDiffs, vDiffsToVal, valToVDiffs, recursiveMergeVal, diffsAt, toTupleDiffs)
import Eval
import Lang exposing (..)
import HTMLValParser
import HTMLParser
import LangParserUtils
import LangUtils exposing (..)
import Utils exposing (reverseInsert, fixtailrec)
import Syntax exposing (Syntax)
import LeoParser as Parser
import Results exposing (Results, ok1, oks)
import LazyList exposing (LazyList)
import LangTools exposing (..)
import ImpureGoodies exposing (whitespaceFromMetadata, whitespaceToMetadata)
import Set exposing (Set)
import Dict exposing (Dict)
import ValUnparser exposing (strVal_, strOp, strLoc)
import ParserUtils
import ValBuilder as Vb
import ValUnbuilder as Vu
import UpdateRegex
import Regex
import LangSvg
import Lazy
import LeoUnparser


builtinEnv =
  [ ("error", builtinVal "EvalUpdate.error" <| VFun "error" ["msg"] (oneArg "error" <| \arg ->
      case arg.v_ of
        VBase (VString s) -> Err s
        _ -> Err <| valToString arg
    ) Nothing)
  , ("parseHTML", HTMLValParser.htmlValParser)
   -- TODO: This && evaluates both sides, can we have something that tries only the first one?
  , ("&&", builtinVal "EvalUpdate.&&" <| VFun "&&" ["left", "right"] (twoArgs "&&" <| \left right ->
         case left.v_ of
           VBase (VBool True) -> Ok (right, [])
           VBase (VBool False) -> Ok (left, [])
           _ -> Err <| "&& expects two booleans, got " ++ valToString left
     ) <| Just <| twoArgsUpdate "&&" <| \left right oldVal newVal diffs ->
       case (oldVal.v_, newVal.v_) of
         (VBase (VBool True), VBase (VBool False)) -> -- At least one of the two must become false.
           oks [([newVal, oldVal], [(0, VConstDiffs)]),
                ([oldVal, newVal], [(1, VConstDiffs)]),
                ([newVal, newVal], [(0, VConstDiffs), (1, VConstDiffs)])]
         (VBase (VBool False), VBase (VBool True)) -> -- Both need to become true
           let leftDiff = case left.v_ of
              VBase (VBool True) -> []
              _ -> [(0, VConstDiffs)]
           in
           let rightDiff = case right.v_ of
             VBase (VBool True) -> []
             _ -> [(1, VConstDiffs)]
           in
           ok1 ([newVal, newVal], leftDiff ++ rightDiff)
         _ ->
           ok1 ([left, right], [])
     )
  , ("||", builtinVal "EvalUpdate.||" <| VFun "||" ["left", "right"] (twoArgs "||" <| \left right ->
         case left.v_ of
           VBase (VBool True) -> Ok (left, [])
           VBase (VBool False) -> Ok (right, [])
           _ -> Err <| "|| expects two booleans, got " ++ valToString left
     ) <| Just <| twoArgsUpdate "&&" <| \left right oldVal newVal diffs ->
      case (oldVal.v_, newVal.v_) of
        (VBase (VBool False), VBase (VBool True)) -> -- At least one of the two must become True.
          oks [([newVal, oldVal], [(0, VConstDiffs)]),
               ([oldVal, newVal], [(1, VConstDiffs)]),
               ([newVal, newVal], [(0, VConstDiffs), (1, VConstDiffs)])]
        (VBase (VBool True), VBase (VBool False)) -> -- Both need to become False
          let leftDiff = case left.v_ of
             VBase (VBool False) -> []
             _ -> [(0, VConstDiffs)]
          in
          let rightDiff = case right.v_ of
            VBase (VBool False) -> []
            _ -> [(1, VConstDiffs)]
          in
          ok1 ([newVal, newVal], leftDiff ++ rightDiff)
        _ ->
          ok1 ([left, right], []))
  , ("<=", builtinVal "EvalUpdate.<=" <| VFun "<=" ["left", "right"] (twoArgs "<=" <| \left right ->
         case (left.v_, right.v_) of
           (VConst _ (n1, _), VConst _ (n2, _))  -> Ok (replaceV_ left <| VBase (VBool (n1 <= n2)), [])
           (VBase (VString n1), VBase (VString n2))  -> Ok (replaceV_ left <| VBase (VBool (n1 <= n2)), [])
           _ -> Err <| "<= expects two numbers, got " ++ valToString left ++ " and " ++ valToString right
     ) Nothing)
  , (">=", builtinVal "EvalUpdate.>=" <| VFun ">=" ["left", "right"] (twoArgs ">=" <| \left right ->
         case (left.v_, right.v_) of
           (VConst _ (n1, _), VConst _ (n2, _))  -> Ok (replaceV_ left <| VBase (VBool (n1 >= n2)), [])
           (VBase (VString n1), VBase (VString n2))  -> Ok (replaceV_ left <| VBase (VBool (n1 >= n2)), [])
           _ -> Err <| ">= expects two numbers, got " ++ valToString left ++ " and " ++ valToString right
     ) Nothing)
  , (">", builtinVal "EvalUpdate.>" <| VFun ">" ["left", "right"] (twoArgs ">" <| \left right ->
         case (left.v_, right.v_) of
           (VConst _ (n1, _), VConst _ (n2, _))  -> Ok (replaceV_ left <| VBase (VBool (n1 > n2)), [])
           (VBase (VString n1), VBase (VString n2))  -> Ok (replaceV_ left <| VBase (VBool (n1 > n2)), [])
           _ -> Err <| "> expects two numbers, got " ++ valToString left ++ " and " ++ valToString right
     ) Nothing)
  , ("/=", builtinVal "EvalUpdate./=" <| VFun "/=" ["left", "right"] (twoArgs "/=" <| \left right ->
         case (left.v_, right.v_) of
           (VConst _ (n1, _), VConst _ (n2, _))  -> Ok (replaceV_ left <| VBase (VBool (n1 /= n2)), [])
           (VBase (VString n1), VBase (VString n2))  -> Ok (replaceV_ left <| VBase (VBool (n1 /= n2)), [])
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
           Eval.doEval Eval.withoutParentsProvenanceWidgets Syntax.Leo env (eApp (eVar "right") [eApp (eVar "left") [eVar "x"]]) |> Result.map Tuple.first
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
         Eval.doEval Eval.withoutParentsProvenanceWidgets Syntax.Leo env (eApp (eVar "left") [eApp (eVar "right") [eVar "x"]]) |> Result.map Tuple.first
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
              Syntax.parser Syntax.Leo s
              |> Result.mapError (ParserUtils.showError)
              |> Result.andThen (\prog ->
                  Eval.doEval Eval.withoutParentsProvenanceWidgets Syntax.Leo (env ++ builtinEnv) prog
                )
              |> Result.map (\((v, ws), env) -> v)
              |> Vb.result Vb.identity (Vb.fromVal program)
              |> (\x -> Ok (x, []))
          _ -> Err <| "evaluate expects a List (String, values) and a program as a string, got " ++ LangUtils.valToString penv ++ " and " ++ LangUtils.valToString program
    ) <| Just <| twoArgsUpdate "__evaluate__" <| \oldpEnv oldProgram oldValr newValr d ->
          case (Vu.list (Vu.tuple2 Vu.string Vu.identity) oldpEnv, oldProgram.v_) of
            (Ok env, VBase (VString s)) ->
              let parsed = Syntax.parser Syntax.Leo s in
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
                           let x = Syntax.unparser Syntax.Leo newProg.val in
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
  , ("__evaluateWithCache__", builtinVal "EvalUpdate.__evaluateWithCache__" <| VFun "__evaluateWithCache__" ["environment", "program"] (twoArgs "__evaluate__" <| \penv program ->
      case (Vu.list (Vu.tuple2 Vu.string Vu.identity) penv, program.v_) of
          (Ok env, VBase (VString s)) ->
              Syntax.parser Syntax.Leo s
              |> Result.mapError (ParserUtils.showError)
              |> Result.andThen (\prog ->
                  Eval.doEval Eval.withoutParentsProvenanceWidgets Syntax.Leo (env ++ builtinEnv) prog
                )
              |> Result.map (\((v, ws), env) -> (v, env))
              |> Vb.result (Vb.tuple2 Vb.identity (Vb.list (Vb.tuple2 Vb.string Vb.identity))) (Vb.fromVal program)
              |> (\x -> Ok (x, []))
          _ -> Err <| "evaluate expects a List (String, values) and a program as a string, got " ++ LangUtils.valToString penv ++ " and " ++ LangUtils.valToString program
    ) <| Just <| twoArgsUpdate "__evaluateWithCache__" <| \oldpEnv oldProgram oldValr newValr d ->
          case (Vu.list (Vu.tuple2 Vu.string Vu.identity) oldpEnv, oldProgram.v_) of
            (Ok env, VBase (VString s)) ->
              let parsed = Syntax.parser Syntax.Leo s in
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
                    case (Vu.result (Vu.tuple2 Vu.identity (Vu.list (Vu.tuple2 Vu.string Vu.identity))) oldValr,
                          Vu.result (Vu.tuple2 Vu.identity (Vu.list (Vu.tuple2 Vu.string Vu.identity))) newValr,
                          vDatatypeDiffsGet "_1" d) of
                      (Ok (Ok (oldVal, oldEnvCache)), Ok (Ok (newVal, _)), Just dds) ->
                          case dds of
                            VRecordDiffs dictDiffs ->
                              case (Dict.get "_1" dictDiffs, Dict.get "_2" dictDiffs) of
                                (_, Just _) -> Err <| "Cannot modify the cache of __evaluateWithCache"
                                (Nothing, _) ->
                                  Err <| "Expected VRecordDiffs with 1 element, got " ++ toString dds
                                (Just dd, _) ->
                                  let initEnv = env ++ builtinEnv in
                                  let previousLets = UpdateStack.keepLets initEnv oldEnvCache in
                                  UpdateStack.updateContext "Eval.__evaluateWithCache__" initEnv prog previousLets oldVal newVal dd
                                 |> update
                                 |> Results.filter (\(newEnv, newProg) ->
                                     let envLength = List.length env in
                                     newEnv.changes |> Utils.findFirst (\(i, _) -> i >= envLength) |> Utils.maybeIsEmpty
                                   )
                                 |> Results.andThen (\(newEnv, newProg) ->
                                   let x = Syntax.unparser Syntax.Leo newProg.val in
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
                            _ -> Err <| "Expected VRecordDiffs with diff on variable only, got " ++ toString dds
                      (_, _, Nothing) -> Err <| "Expected VRecordDiffs with 1 element, got " ++ toString d
                      (Err msg, _, _) -> Err msg
                      (_, Err msg, _) -> Err msg
                      (Ok (Err msg), Ok (Err msg2), _) -> if msg == msg2 then ok1 ([oldpEnv, oldProgram], []) else Err <| "Cannot change the error message of __evaluate__ to " ++ msg2
                      (Ok (Err msg), Ok (Ok (x, _)), _) -> Err <| "Cannot change the outpur of __evaluateWithCache__ from error '"++msg++"' to " ++ valToString x ++ "'"
                      (_, Ok (Err msg2), _) -> Err <| "Don't know how to update the result of a correct __evaluateWithCache__ by an error '" ++ msg2 ++ "'"
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
                  Nothing -> Eval.doEval Eval.withoutParentsProvenanceWidgets Syntax.Leo xyEnv xyExp |> Result.map (\((v, _), _) -> v)
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
                        let resultingValue = Vb.result UpdateUtils.updateReturnToVal vb <| Ok <| InputsWithDiffs [(input, Nothing)] in
                        Ok (resultingValue, [])
                      Ok (Just newOutDiffs) ->
                        let basicResult = case update <| updateContext "__updateApp__" xyEnv xyExp [] oldOut newVal newOutDiffs of
                          Err msg -> Err msg
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
                                 Ok <| InputsWithDiffs []
                               else
                                 Err "Only solutions modifying the constant function of __updateApp__"
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
                               Ok <| InputsWithDiffs <| Utils.zip results diffs
                        in Ok (Vb.result UpdateUtils.updateReturnToVal vb basicResult, [])
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
  , ("__mergeHtmlText__", builtinVal "Evalupdate.__mergeHtmlText__" <|
     VFun "__mergeHtmlText__" ["htmlnodeList"] (oneArg "htmlnodeList" <|
       \original ->
         let mergeHtmlText l = case l of
           ha::hb::tail -> case (vHtmlTextUnapply ha, vHtmlTextUnapply hb) of
              (Just a, Just b) -> mergeHtmlText <| Vb.htmlText (Vb.fromVal original) (a ++ b) :: tail
              (Just "", Nothing) ->
                mergeHtmlText <| (hb :: tail)
              _ -> ha :: mergeHtmlText (hb :: tail)
           [ha] -> case vHtmlTextUnapply ha of
              Just "" -> []
              _ -> l
           _ -> l
         in
         case vListUnapply original of
           Just l -> Ok <| (Vb.list Vb.identity (Vb.fromVal original) <| mergeHtmlText l, [])
           Nothing -> Err <| "Expected a list, got " ++ valToString original
       ) <| Just <| oneArgUpdate "htmlnodeList" <| \original oldVal newVal diffs ->
       --let _ = Debug.log ("__mergeHtmlText__'s input diffs: " ++ UpdateUtils.vDiffsToString oldVal newVal diffs) () in
       let aux: Int -> Int -> List Val -> List Val -> List Val -> ListDiffs VDiffs ->
                Results String (List Val, ListDiffs VDiffs) -> Results String (List Val, ListDiffs VDiffs)
           aux originalIndex outputIndex originals oldOutputs newOutputs diffs resAccRevValRevDiffs =
             {-let _ = Debug.log ("aux " ++ toString originalIndex ++ " " ++ toString outputIndex ++ " " ++
               (List.map valToString originals |> String.join ",") ++ " " ++
               (List.map valToString oldOutputs |> String.join ",") ++ " " ++
               (List.map valToString newOutputs |> String.join ",") ++ " " ++
                toString diffs ++ " " ++ ( case resAccRevValRevDiffs of
                  Err msg -> "Err " ++ msg
                  Ok LazyList.Nil -> "Empty list!!"
                  Ok x -> "List of size " ++ (LazyList.toList x |> List.length |> toString)
                )
                ) () in-}
             let skipStepAndOutput originalCount outputCount diffs =
               resAccRevValRevDiffs
               |> Results.map (\(revVals, revDiffs) ->
                  (Utils.reverseInsert (List.take originalCount originals) revVals,
                   revDiffs)
                ) |> aux (originalIndex + originalCount) (outputIndex + outputCount)
                   (List.drop originalCount originals) (List.drop outputCount oldOutputs) (List.drop outputCount newOutputs) diffs
             in
             let skipStep originalCount = skipStepAndOutput originalCount 1 in
             let insertStep count tailDiffs =
               let (inserted, newOutputTail) = Utils.split count newOutputs in
               resAccRevValRevDiffs
               |> Results.map (\(revVals, revDiffs) ->
                  (Utils.reverseInsert inserted revVals,
                   (originalIndex, ListElemInsert count) :: revDiffs)
               ) |> aux originalIndex outputIndex originals oldOutputs newOutputTail tailDiffs
             in
             let deleteStep count originalCount tailDiffs =
               let newDiffs = if count == 1 then tailDiffs else (outputIndex + 1, ListElemDelete (count - 1)) :: tailDiffs in
               resAccRevValRevDiffs
               |> Results.map (\(revVals, revDiffs) ->
                  (revVals,
                   (originalIndex, ListElemDelete originalCount) :: revDiffs)
               ) |> aux (originalIndex + originalCount) (outputIndex + 1) (List.drop originalCount originals) (List.drop 1 oldOutputs) newOutputs newDiffs
             in
             let defaultStep () = -- When the original element has been directly put into the output
               case diffs of
                  [] ->
                    case originals of
                      [] ->
                        resAccRevValRevDiffs
                        |> Results.map (\(revVals, revDiffs) ->
                            (List.reverse revVals, List.reverse revDiffs))
                      _ -> skipStep 1 []
                  (j, textDiffs) :: tailDiffs ->
                    if outputIndex < j then
                      skipStep 1 diffs
                    else -- i == j
                      case textDiffs of
                        ListElemDelete count ->
                          deleteStep count 1 tailDiffs
                        ListElemInsert count ->
                          insertStep count tailDiffs
                        ListElemUpdate d ->
                          resAccRevValRevDiffs
                          |> Results.map (\(revVals, revDiffs) ->
                            (Utils.reverseInsert (List.take 1 newOutputs) revVals,
                             (originalIndex, ListElemUpdate d)::revDiffs)
                          ) |>
                          aux (originalIndex + 1) (outputIndex + 1)
                              (List.drop 1 originals) (List.drop 1 oldOutputs) (List.drop 1 newOutputs) tailDiffs
             in
             let (manyTexts, originalTail) = Utils.splitPrefix vHtmlTextUnapply originals in
             let finalStep () =
               case originals of
                  [] ->
                    resAccRevValRevDiffs
                    |> Results.map (\(revVals, revDiffs) ->
                      (List.reverse revVals, List.reverse revDiffs))
                  _ -> skipStep (List.length manyTexts) []
             in
             if List.length manyTexts == 0 then defaultStep ()
             else if List.all ((==) "") manyTexts then -- This string was deleted, so insertions could result in modifications
               case diffs of
                 [] -> finalStep ()
                 (j, diff) :: tailDiffs ->
                   if outputIndex /= j then -- we skip all these values
                     skipStep (List.length manyTexts) diffs
                   else
                   case diff of
                     ListElemDelete count ->
                       skipStepAndOutput (List.length manyTexts) 0 diffs -- The deletion is for a further node
                     ListElemUpdate d ->
                       skipStepAndOutput (List.length manyTexts) 0 diffs -- The deletion is for a further node
                     ListElemInsert count -> -- Insertion of text nodes at this point could be for
                       if count == 0 then
                         skipStepAndOutput 0 0 tailDiffs
                       else
                         case newOutputs of
                           [] -> Err <| "Inconsistent diffs, says ListElemInsert but got no new output"
                           newHead :: newTail ->
                             let finalTailDiffs = -- One insertion is going to become an update
                                   if count == 1 then tailDiffs
                                   else ((j, ListElemInsert (count - 1)) :: tailDiffs) in
                             case vHtmlTextUnapply newHead of
                               Nothing -> -- Insertion of elements could happen before the modification of text
                                 insertStep 1 finalTailDiffs
                               Just newStr ->
                                 UpdateUtils.reverseStringConcatenationMultiple manyTexts newStr [StringUpdate 0 0 (String.length newStr)]
                                 |> Results.andThen (                    fixtailrec resAccRevValRevDiffs <| \resAccRevValRevDiffs ->fixtailrec originalIndex <| \originalIndex ->
                                   \(newMany, newManyDiffs) ->
                                     resAccRevValRevDiffs
                                     |> Results.map (                    fixtailrec newMany <| \newMany -> fixtailrec newManyDiffs <| \newManyDiffs ->
                                       \(revVals, revDiffs) ->
                                         let newRevVals =
                                               Utils.reverseInsert
                                                 (List.map (Vb.htmlText (Vb.fromVal original)) newMany) revVals
                                             newRevDiffs =
                                               Utils.reverseInsert
                                                 (Utils.zipWithIndex newManyDiffs
                                                  |> List.concatMap (\(d, index) ->
                                                    if d == [] then []
                                                    else
                                                      [(originalIndex + index, ListElemUpdate (vHtmlTextDiffs <| VStringDiffs d))]))
                                                 revDiffs
                                         in
                                         (newRevVals, newRevDiffs)
                                     ))
                                 |> aux (originalIndex + List.length manyTexts) outputIndex originalTail oldOutputs newTail finalTailDiffs
             else
               -- All these texts were concatenated in the output.
               -- The first value of outputs is the concatenation of manyTexts
               case diffs of
                 [] -> finalStep ()
                 (j, diff) :: tailDiffs ->
                   if outputIndex /= j then -- we skip all these values
                     skipStep (List.length manyTexts) diffs
                   else
                   case diff of
                     ListElemDelete count ->
                       deleteStep count (List.length manyTexts) tailDiffs
                     ListElemInsert count ->
                       insertStep count tailDiffs
                     ListElemUpdate textDiffs ->
                       case (oldOutputs, newOutputs) of
                         (_, []) -> Err <| "Inconsistent diffs, says ListElemUpdate but got no new output"
                         ([], _) -> Err <| "Inconsistent diffs, says ListElemUpdate but got no old output"
                         (_ :: oldTail, newHead::newTail) ->
                           case (vHtmlTextUnapply newHead, vHtmlTextDiffsUnapply textDiffs) of
                             (Just newStr, Just strDiffs) ->
                               --let _ = Debug.log "manyTexts" manyTexts in
                               --let _ = Debug.log "newStr" newStr in
                               --let _ = Debug.log "strDiffs" strDiffs in
                               UpdateUtils.reverseStringConcatenationMultiple manyTexts newStr strDiffs
                               |> Results.andThen (
                                        fixtailrec resAccRevValRevDiffs <| \resAccRevValRevDiffs ->
                                        fixtailrec originalIndex <| \originalIndex ->
                                   \(newMany, newManyDiffs) ->
                                     --let _ = Debug.log "newMany" newMany in
                                     --let _ = Debug.log "newManyDiffs" newManyDiffs in
                                     resAccRevValRevDiffs
                                     |> Results.map (
                                           fixtailrec newMany <| \newMany ->
                                           fixtailrec newManyDiffs <| \newManyDiffs ->
                                       \(revVals, revDiffs) ->
                                         let newRevVals =
                                               Utils.reverseInsert
                                                 (List.map (Vb.htmlText (Vb.fromVal original)) newMany) revVals
                                             newRevDiffs =
                                               Utils.reverseInsert
                                                 (Utils.zipWithIndex newManyDiffs
                                                  |> List.concatMap (\(d, index) ->
                                                    if d == [] then []
                                                    else
                                                      [(originalIndex + index, ListElemUpdate (vHtmlTextDiffs <| VStringDiffs d))]))
                                                 revDiffs
                                         in
                                         (newRevVals, newRevDiffs)
                                       ))
                               |> aux (originalIndex + List.length manyTexts) (outputIndex + 1) originalTail oldTail newTail tailDiffs
                             (_, Nothing) ->
                               Err <| "In a html text node, cannot update anything else than the text itself. Got " ++ toString textDiffs
                             (Nothing, _) ->
                               Err <| "In a html text node, cannot update with anything else than a text node. Got new value " ++ valToString newHead
       in
       case (vListUnapply original, vListUnapply oldVal, vListUnapply newVal, diffs) of
         (Just originals, Just oldOutputs, Just newOutputs, VListDiffs ldiffs) ->

          aux 0 0 originals oldOutputs newOutputs ldiffs (ok1 ([], []))
          |> Results.map (\(l, ldiffs) ->
            let finalDiffs = if ldiffs == [] then [] else [(0, VListDiffs ldiffs)] in
            ([Vb.list Vb.identity (Vb.fromVal original) l], finalDiffs)
          )
         _ -> Err <| "Expected lists and listdiffs to update __mergeHtmlText__, got " ++
           valToString original ++ ", " ++ valToString oldVal ++ ", " ++ valToString newVal ++ ", " ++ toString diffs
    )
  , ("__htmlEntity__", builtinVal "EvalUpdate.__htmlEntity__" <|
    VFun "__htmlEntity__" ["entityRendered", "entity"] (twoArgs "__htmlEntity__" <| \entityRendered entity ->
      Ok (Vb.list (Vb.viewtuple2 Vb.string Vb.identity) (Vb.fromVal entityRendered) [("TEXT", entityRendered)], [])
    ) <| Just <| twoArgsUpdate "__htmlEntity__" <| \entityRendered entity oldVal newVal diffs ->
      case (entityRendered.v_, entity.v_, Vu.list (Vu.viewtuple2 Vu.string Vu.string) newVal) of
        (VBase (VString entityRenderedS), VBase (VString entityS), Ok newValsList) ->
          let newValS = List.map (\(x, n) -> n) newValsList |> String.join "" in
          let escapedNewValS= ImpureGoodies.htmlescape newValS in
          let newEntity = replaceV_ newVal <| VBase (VString escapedNewValS) in
          let newEntityRenderedS = newValS in
          let newEntityRendered = replaceV_ newVal <| VBase <| VString newEntityRenderedS in
          ok1 ([newEntityRendered, newEntity],
            [(0, VStringDiffs [StringUpdate 0 (String.length entityRenderedS) (String.length newEntityRenderedS)]),
             (1, VStringDiffs [StringUpdate 0 (String.length entityS) (String.length escapedNewValS)])])
        _ ->
          Err <| "Expected strings as arguments and [[\"TEXT\", _]] as return of __htmlEntity__, got __htmlEntity__ " ++ valToString entityRendered ++ " " ++ valToString entity ++ " updated by " ++ valToString newVal
    )
  , ("__htmlStrEntity__", builtinVal "EvalUpdate.__htmlStrEntity__" <|
    VFun "__htmlStrEntity__" ["entityRendered", "entity"] (twoArgs "__htmlStrEntity__" <| \entityRendered entity ->
      Ok (entityRendered, [])
    ) <| Just <| twoArgsUpdate "__htmlStrEntity__" <| \entityRendered entity oldVal newVal diffs ->
      case (entityRendered.v_, entity.v_, newVal.v_) of
        (VBase (VString entityRenderedS), VBase (VString entityS), VBase (VString newValS)) ->
          let escapedNewValS= newValS in
          let newEntity = replaceV_ newVal <| VBase (VString escapedNewValS) in
          let newEntityRenderedS = newValS in
          let newEntityRendered = replaceV_ newVal <| VBase <| VString newEntityRenderedS in
          ok1 ([newEntityRendered, newEntity],
            [(0, VStringDiffs [StringUpdate 0 (String.length entityRenderedS) (String.length newEntityRenderedS)]),
             (1, VStringDiffs [StringUpdate 0 (String.length entityS) (String.length escapedNewValS)])])
        _ -> Err <| "Expected strings as arguments and as return of __htmlStrEntity__, got __htmlStrEntity__ " ++ valToString entityRendered ++ ", " ++ valToString entity ++ " updated by " ++ valToString newVal
    )
  , ("__htmlRawAttribute__", builtinVal "EvalUpdate.__htmlRawAttribute__" <|
     VFun "__htmlRawAttribute__" ["content"] (oneArg "__htmlRawAttribute__" <| \arg ->
       Ok (arg, [])
     ) <| Just <| oneArgUpdate "__htmlRawAttribute__" <| \arg oldVal newVal diffs ->
       ok1 ([newVal], [(0, diffs)]))
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
  , ("valToHTMLSource", builtinVal "EvalUpdate.valToHTMLSource" <|
       VFun "valToHTMLSource" ["htmlNode"] (oneArg "valToHTMLSource" <| \original ->
          LangSvg.valToHTMLSource HTMLParser.HTML original
          |> Result.map (\str -> replaceV_ original (VBase (VString str)))
          |> Result.map (flip (,) [])
       ) <| Just <| oneArgUpdate "valToHTMLSource" <| \original oldOutput newOutput diffs ->
          case Vu.string newOutput |> Result.andThen (LangSvg.htmlSourceToVal HTMLParser.HTML) of
            Err msg -> Err msg
            Ok result ->
              case defaultVDiffs original result of
                Err msg -> Err <| "Error while computing the diffs for valToHTMLSource: " ++ msg
                Ok ds ->
                  Ok (ds |> LazyList.map (\mbd -> case mbd of
                    Nothing -> ([result], [])
                    Just d -> ([result], [(0, d)])))
       )
  , ("typeof", builtinVal "EvalUpdate.typeof" <|
       VFun "typeof" ["value"] (oneArg "valToHTMLSource" <| \original ->
         let typ = case original.v_ of
               VClosure _ _ _ _ -> "function"
               VFun _ _ _ _ -> "function"
               VRecord _  -> "object"
               VDict _ -> "dict"
               VList _ -> "list"
               VConst _ _ -> "number"
               VBase (VBool _) -> "boolean"
               VBase (VString _) -> "string"
               VBase (VNull) -> "undefined"
         in
         Ok (builtinVal "EvalUpdate.typeof.result" (VBase (VString typ)), [])
       ) Nothing)
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

eval env e = Eval.doEval Eval.withoutParentsProvenanceWidgets Syntax.Leo env e |> Result.map Tuple.first
update updateStack = Update.update LazyList.Nil LazyList.Nil updateStack

preludeEnv =
  Utils.fromOk "Eval.preludeEnv" <|
  Result.map Tuple.second <|
  (Eval.eval [] (Eval.evalContext Eval.withParentsProvenanceWidgets Syntax.Leo builtinEnv [] Parser.prelude))

run : Syntax -> Exp -> Result String (Val, Widgets)
run syntax e =
-- doEval syntax initEnv e |> Result.map Tuple.first
  -- ImpureGoodies.logTimedRun "Eval.run" <| \() ->
    Eval.doEval Eval.withParentsProvenanceWidgets syntax preludeEnv e |> Result.map Tuple.first

runWithEnv : Syntax -> Exp -> Result String ((Val, Widgets), Env)
runWithEnv syntax e =
-- doEval syntax initEnv e |> Result.map Tuple.first
  --ImpureGoodies.logTimedRun "Eval.run" <| \() ->
    Eval.doEval Eval.withParentsProvenanceWidgets syntax preludeEnv e

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
        Ok (LazyList.Cons Nothing _ ) ->
          let _ = ImpureGoodies.log "No difference observed in the output." in
          ok1 (UpdatedEnv.original preludeEnv, UpdatedExp oldExp Nothing)
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

preludeIdentifiersList = preludeEnv |> List.map Tuple.first
preludeIdentifiers = preludeIdentifiersList |> Set.fromList

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
    LangTools.assignUniqueNames_ program initialUsedNames Dict.empty
  in
  (newProgram, newNameToOldName)


-- What variable names are in use at any of the given locations?
-- For help finding unused names during synthesis.
visibleIdentifiersAtEIds : Exp -> Set.Set EId -> Set.Set Ident
visibleIdentifiersAtEIds program eids =
  let programIdents = visibleIdentifiersAtPredicateNoPrelude program (\exp -> Set.member (expEId exp) eids) in
  Set.union programIdents preludeIdentifiers


newVariableVisibleTo : EId -> Ident -> Int -> Exp -> List EId -> Exp -> (Ident, Exp)
newVariableVisibleTo insertedLetEId suggestedName startingNumberForNonCollidingName boundExp observerEIds program =
  let
    newName =
      nonCollidingName suggestedName startingNumberForNonCollidingName (visibleIdentifiersAtEIds program (Set.fromList observerEIds))
    eidToWrap =
      deepestCommonAncestorWithNewlineOrELet program (\exp -> List.member (expEId exp) observerEIds) |> expEId
    newProgram =
      program
      |> mapExpNode
          eidToWrap
          (\expToWrap ->
            newLetFancyWhitespace insertedLetEId False (pVar newName) boundExp expToWrap program
          )
  in
  (newName, newProgram)

visibleIdentifiersAtEIdBindingNum: Exp -> (InsertionMethod, (EId, BindingNumber)) -> List Ident
visibleIdentifiersAtEIdBindingNum  program insertionPosition =
  visibleIdentifiersAtEIdBindingNumNoPrelude program insertionPosition ++ preludeIdentifiersList


identifiersVisibleAtProgramEnd : Exp -> Set.Set Ident
identifiersVisibleAtProgramEnd program =
  let lastEId = expEId <| lastExp program in
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
  Syntax.parser Syntax.Leo s
  |> Result.mapError ParserUtils.showError

unparse: Exp -> String
unparse e = Syntax.unparser Syntax.Leo e

evalExp: Exp -> Result String (Val, Env)
evalExp exp = evaluateRaw [] exp

evalExpWithoutCache: Exp -> Result String Val
evalExpWithoutCache exp = evalExp exp |> Result.map Tuple.first

-- Same as updateExp, but with an additional environment that can change.
updateEnvExp: Env -> Exp -> (Val, Env) -> Val -> Results String (Env, Exp)
updateEnvExp env oldExp (oldVal, oldEnvCache) newVal =
  let thediffs = UpdateUtils.defaultVDiffs oldVal newVal
  in
  case thediffs of
    Err msg -> Err msg
    Ok (LazyList.Nil ) -> Err "[Internal error] expected a diff or an error, got Nil"
    Ok (LazyList.Cons Nothing _ ) -> ok1 (env, oldExp)
    Ok ll ->
        Ok (ll |> LazyList.filterMap identity) |> Results.andThen (\diffs ->
         let initEnv = env ++ preludeEnv in
         let previousLets = UpdateStack.keepLets initEnv oldEnvCache in
         (update <| updateContext "initial update" initEnv oldExp previousLets oldVal newVal diffs)) |>
         Results.filter (\(newEnv, newExp) ->
           case Utils.maybeLast newEnv.changes of
             Nothing -> True
             Just (i, d) -> i < List.length env) |>
         Results.map (\(newEnv, newExp) -> (List.take (List.length env) newEnv.val, newExp.val))

updateExp: Exp -> (Val, Env) -> Val -> Results String Exp
updateExp oldExp oldValEnvCache newVal =
  updateEnvExp [] oldExp oldValEnvCache newVal
  |> Results.map Tuple.second

updateExpWithoutCache: Exp -> Val -> Results String Exp
updateExpWithoutCache oldExp newVal =
  evaluateRaw [] oldExp
  |> Results.fromResult
  |> Results.andThen (\oldValEnvCache ->
    updateExp oldExp oldValEnvCache newVal
   )

valToNative: Val -> Result String a
valToNative v = case v.v_ of
  VBase (VString x) -> Ok <| ImpureGoodies.hideType x
  VConst _ (i, _)   -> Ok <| ImpureGoodies.hideType i
  VBase (VBool b)   -> Ok <| ImpureGoodies.hideType b
  VBase (VNull)     -> Ok <| ImpureGoodies.hideType 0
  VList l           -> List.map valToNative l |> Utils.projOk |> Result.map toArray
  VRecord d         -> case Utils.projOk <| List.map (\(x, y) -> valToNative y |> Result.map ((,) x)) <| Dict.toList d of
      Ok r -> Ok <| ImpureGoodies.keyPairsToNativeRecord <| r
      Err msg -> Err msg
  VFun name args ev up ->
    Ok <| ImpureGoodies.hideType <| (\x ->
      case Result.andThen (Tuple.first >> valToNative) <| ev [nativeToVal (Vb.fromVal v) x] of
        Ok r -> ImpureGoodies.hideType r
        Err msg -> Debug.crash <| "Native version of " ++ name ++ " function crashed with : " ++ msg)
  VClosure recIdents pats body closureEnv ->
    Ok <| ImpureGoodies.hideType (\x ->
      let arg = nativeToVal (Vb.fromVal v) x in
      case evaluateRawWithoutCache [("fun", v), ("arg", arg)] (eApp (eVar "fun") [eVar "arg"]) |> Result.andThen valToNative of
        Err msg -> Debug.crash <| "Native version of closure crashed with : " ++ msg
        Ok result -> result)
  _ -> Err <| "Don't know how to convert dictionaries to native values"

nativeToVal: Vb.Vb -> a -> Val
nativeToVal vb v =
  ImpureGoodies.fromNative v
    (\s -> Vb.string vb s)
    (\n -> Vb.num vb n)
    (\b -> Vb.bool vb b)
    (\l -> Vb.list nativeToVal vb l)
    (\r -> Vb.record nativeToVal vb (Dict.fromList r))
    (\f -> vb (VFun "anonymous" ["anonArg"] (oneArg "anonymous" <| \v ->
      case valToNative v of
        Ok nativeV -> f nativeV |> nativeToVal vb |> flip (,) [] |> Ok
        Err msg -> Err msg) Nothing))

objectToEnv: objectAsEnvOfConsts -> Result String Env
objectToEnv objectAsEnvOfConsts =
  let envAsVal = nativeToVal (builtinVal "EvalUpdate.nativeToVal") objectAsEnvOfConsts in
  case envAsVal.v_ of
      VRecord d ->
        Ok <| Dict.toList d
      v -> Err <| "Environment not interpretable: " ++ valToString envAsVal

envToObject: Env -> Result String objectAsEnvOfConsts
envToObject env =
  valToNative (Vb.record Vb.identity (builtinVal "EvalUpdate.nativeToVal") (Dict.fromList env))

evaluateRawWithoutCache: Env -> Exp -> Result String Val
evaluateRawWithoutCache env exp =
  evaluateRaw env exp
  |> Result.map Tuple.first

-- Return the resulting environment as well
evaluateRaw: Env -> Exp -> Result String (Val, Env)
evaluateRaw env exp =
   Eval.doEval Eval.withoutParentsProvenanceWidgets Syntax.Leo (env ++ preludeEnv) exp
  |> Result.map (\((val, ws), env) -> (val, env))

evaluateEnv: objectAsEnvOfConsts -> Exp -> Result String (Val, Env)
evaluateEnv objectAsEnvOfConsts exp =
  objectToEnv objectAsEnvOfConsts
  |> Result.andThen (flip evaluateRaw exp)

evaluateString: String -> Result String (Val, Env)
evaluateString stringSource =
  parse stringSource
  |> Result.andThen (evaluateRaw [])

evaluateEnvString: objectAsEnvOfConsts -> String -> Result String (Val, Env)
evaluateEnvString objectAsEnvOfConsts stringSource =
  objectToEnv objectAsEnvOfConsts
  |> Result.andThen (\env ->
    parse stringSource
    |> Result.andThen (evaluateRaw env))

updateRaw: Env -> Exp -> Val -> Results String (Env, Exp)
updateRaw env exp newVal =
  evaluateRaw env exp
  |> Result.andThen (\oldValEnvCache ->
    updateEnvExp env exp oldValEnvCache newVal)

resultWithNativeEnv = LazyList.map (\(newEnv, newExp) ->
        (envToObject newEnv |> Utils.fromOk "EvalUpdate.env to native javascript", newExp))

resultStringWithNativeEnv = LazyList.map (\(newEnv, newExp) ->
        (envToObject newEnv |> Utils.fromOk "EvalUpdate.env to native javascript", unparse newExp))

updateEnv: objectAsEnvOfConsts -> Exp -> Val -> Results String (objectAsEnvOfConsts, Exp)
updateEnv objectAsEnvOfConsts exp newVal =
  objectToEnv objectAsEnvOfConsts
  |> Result.andThen (\env ->
    updateRaw env exp newVal
    |> Result.map resultWithNativeEnv
    )

updateEnvWithOld: objectAsEnvOfConsts -> Exp -> (Val, Env) -> Val -> Results String (objectAsEnvOfConsts, Exp)
updateEnvWithOld objectAsEnvOfConsts exp oldValEnvCache newVal =
  objectToEnv objectAsEnvOfConsts
  |> Result.andThen (\env ->
    updateEnvExp env exp oldValEnvCache newVal
    |> Result.map resultWithNativeEnv
    )

updateString: String -> Val -> Results String String
updateString strSource newVal =
    parse strSource
    |> Result.andThen (\exp ->
    updateRaw [] exp newVal
    |> Result.map (LazyList.map (\(newenv, newExp) ->
              unparse newExp)))

updateStringWithOld: String -> (Val, Env)-> Val -> Results String String
updateStringWithOld strSource  oldValEnvCache newVal =
    parse strSource
    |> Result.andThen (\exp ->
    updateEnvExp [] exp oldValEnvCache newVal
    |> Result.map (LazyList.map (\(newenv, newExp) -> unparse newExp)))

updateEnvString: objectAsEnvOfConsts -> String -> Val -> Results String (objectAsEnvOfConsts2, String)
updateEnvString objectAsEnvOfConsts strSource newVal =
  objectToEnv objectAsEnvOfConsts
  |> Result.andThen (\env ->
    parse strSource
    |> Result.andThen (\exp ->
    updateRaw env exp newVal
    |> Result.map resultStringWithNativeEnv
    ))

updateEnvStringWithOld: objectAsEnvOfConsts -> String -> (Val, Env) -> Val -> Results String (objectAsEnvOfConsts2, String)
updateEnvStringWithOld objectAsEnvOfConsts strSource oldValEnvCache newVal =
  objectToEnv objectAsEnvOfConsts
  |> Result.andThen (\env ->
    parse strSource
    |> Result.andThen (\exp ->
    updateEnvExp env exp oldValEnvCache newVal
    |> Result.map resultStringWithNativeEnv
    ))
type alias StringObjEnvType envA envB envC = {
    evaluate: envA -> String -> Result String (Val, Env),
    update: envB -> String -> (Val, Env) -> Val -> Results String (envC, String),
    evaluateWithoutCache: envA -> String -> Result String Val,
    updateWithoutCache: envB -> String -> Val -> Results String (envC, String)
  }

stringObjEnv: StringObjEnvType envA envB envC
stringObjEnv = {
    -- JSObj -> StringSource-> Result StringError Val
    evaluate = evaluateEnvString, -- Takes an object (environment) and a string, returns a Val
    update = updateEnvStringWithOld,
    evaluateWithoutCache = \env source -> evaluateEnvString env source |> Result.map Tuple.first,
    -- JSObj -> StringSource-> Val -> Result StringError (LazyList (JSObj, String))
    updateWithoutCache = updateEnvString -- Takes an object (environment), a string, a new val, returns a list of pairs of objects (environment) and strings (programs)
  }

type alias StringType envA envB envC  = {
    evaluate: String -> Result String (Val, Env),
    update: String -> (Val, Env) -> Val -> Results String String,
    evaluateWithoutCache: String -> Result String Val,
    updateWithoutCache: String -> Val -> Results String String,
    objEnv: StringObjEnvType envA envB envC
  }
string: StringType envA envB envC
string = {
    -- StringSource -> Result StringError Val
    evaluate = evaluateString, -- Takes a string, returns a Val
    evaluateWithoutCache = evaluateString >> Result.map Tuple.first,
    -- StringSource -> Val -> Result StringError (LazyList StringSource)
    update = updateStringWithOld, -- Takes a string (program), a new val, returns a list of new strings (programs=
    updateWithoutCache= updateString,
    objEnv = stringObjEnv
  }

type alias ObjEnvType envA envB envC = {
    evaluate: envA -> Exp -> Result String (Val, Env),
    update: envB -> Exp -> (Val, Env) -> Val -> Results String (envB, Exp),
    evaluateWithoutCache: envA -> Exp -> Result String Val,
    updateWithoutCache: envB -> Exp -> Val -> Results String (envB, Exp),
    string: StringObjEnvType envA envB envC
  }
objEnv: ObjEnvType envA envB envC
objEnv = {
    evaluate = evaluateEnv,
    update = updateEnvWithOld,
    evaluateWithoutCache = \envA exp -> evaluateEnv envA exp |> Result.map Tuple.first,
    updateWithoutCache = updateEnv,
    string = stringObjEnv
  }

type alias LazyListType a = {
  nonEmpty: LazyList a -> Bool,
  isEmpty: LazyList a -> Bool,
  head: LazyList a -> a,
  tail: LazyList a -> LazyList a
  }
lazyList: LazyListType a
lazyList = {
  nonEmpty = \l -> case l of
    LazyList.Nil -> False
    _ -> True,
  isEmpty = \l -> case l of
    LazyList.Nil -> True
    _ -> False,
  head = \l -> case l of
    LazyList.Nil -> Debug.crash "Head of empty lazy list"
    LazyList.Cons a lt -> a,
  tail = \l -> case l of
    LazyList.Nil -> Debug.crash "Tail of empty lazy list"
    LazyList.Cons a lt -> Lazy.force lt
  }

-- Javascript API to construct and deconstruct SNS expressions
type alias LangApi array_metadata_name_Pat array_Declaration array_Pat array_Branch array_Exp metadata metadata2 value = {
  eBase: metadata -> value -> Exp,
  eVar: metadata -> String -> Exp,
  eFun: metadata -> array_Pat -> Exp -> Exp,
  eApp: metadata -> Exp -> array_Exp -> Exp,
  eOp: metadata -> String -> array_Exp -> Exp,
  eList: metadata -> array_Exp -> Exp,
  eCons: metadata -> Exp -> Exp -> Exp,
  eIf: metadata -> Exp -> Exp -> Exp -> Exp,
  eCase: metadata -> Exp -> array_Branch -> Exp,
  eLet: metadata -> Declarations -> Exp -> Exp,
  eParens: metadata -> Exp -> String -> Exp,
  eRecord: metadata -> Declarations -> Exp,
  eRecordExtend: metadata -> Exp -> Declarations -> Exp,
  eSelect: metadata -> Exp -> String -> Exp,
  letTypeAlias: metadata -> Pat -> Type -> LetType,
  letTypeData: metadata -> Pat -> Type -> LetType,
  letAnnotation: metadata -> Pat -> Type -> LetAnnotation,
  letExp: metadata -> Pat -> Exp -> LetExp,
  declType: LetType -> Declaration,
  declExp: LetExp -> Declaration,
  declAnnotation: LetAnnotation -> Declaration,
  declarations: metadata -> array_Declaration -> Declarations,
  --declarations: metadata -> PrintOrder -> GroupsOf LetType -> List LetAnnotation -> GroupsOf LetExp -> Declarations,
  branch: metadata -> Pat -> Exp -> Branch,
  pVar: metadata -> String -> Pat,
  pBase: metadata -> value -> Pat,
  pWildcard: metadata -> Pat,
  pList: metadata -> array_Pat -> Pat,
  pCons: metadata -> Pat -> Pat -> Pat,
  pRecord: metadata -> array_metadata_name_Pat {- Array {metadata, name, pat} -} -> Pat,
  pParens: metadata -> Pat -> Pat,
  pAs: metadata -> Pat -> Pat -> Pat,

  unapply: {
    eBase: Exp -> Maybe (metadata, value),
    eVar: Exp -> Maybe (metadata,  String),
    eFun: Exp -> Maybe (metadata,  array_Pat,  Exp),
    eApp: Exp -> Maybe (metadata,  Exp,  array_Exp),
    eOp: Exp -> Maybe (metadata,  String, array_Exp),
    eList: Exp -> Maybe (metadata,  array_Exp),
    eCons: Exp -> Maybe (metadata,  Exp,  Exp),
    eIf: Exp -> Maybe (metadata,  Exp,  Exp,  Exp),
    eCase: Exp -> Maybe (metadata,  Exp,  array_Branch),
    eLet: Exp -> Maybe (metadata,  Declarations,  Exp),
    eParens: Exp -> Maybe (metadata,  Exp, String),
    eRecord: Exp -> Maybe (metadata,  Declarations),
    eRecordExtend: Exp -> Maybe (metadata,  Exp,  Declarations),
    eSelect: Exp -> Maybe (metadata,  Exp,  String),
    letTypeAlias: LetType-> Maybe (metadata, Pat, Type),
    letTypeData: LetType -> Maybe (metadata, Pat, Type),
    letAnnotation: LetAnnotation -> (metadata, Pat, Type),
    letExp: LetExp -> (metadata, Pat, Exp),
    declType: Declaration -> Maybe LetType,
    declExp: Declaration -> Maybe LetExp,
    declAnnotation: Declaration -> Maybe LetAnnotation,
    declarations: Declarations -> array_Declaration,
    -- declarations: Declarations -> (metadata, PrintOrder, GroupsOf LetType, List LetAnnotation, GroupsOf LetExp),
    branch: Branch -> (metadata, Pat, Exp),
    pVar: Pat -> Maybe (metadata,  String),
    pBase: Pat -> Maybe (metadata,  value),
    pWildcard: Pat -> Maybe (metadata),
    pList: Pat -> Maybe (metadata,  array_Pat),
    pCons: Pat -> Maybe (metadata,  Pat,  Pat),
    pRecord: Pat -> Maybe (metadata,  List (metadata2, String, Pat)),
    pParens: Pat -> Maybe (metadata,  Pat),
    pAs: Pat -> Maybe (metadata,  Pat,  Pat)
    }
  }

wsMetadata: (WS -> x) -> (metadata -> x)
wsMetadata callback metadata =
  let wsb = ws <| whitespaceFromMetadata metadata in
  callback wsb

fromArray = ImpureGoodies.fromNativeArray
toArray = ImpureGoodies.toNativeArray
mdOf = whitespaceToMetadata << .val
-- This API is to create programs from scratch (e.g. use an existing parser)
-- Expressions should not be meant to be unparsed, although they could
--lang: LangApi metadata metadata2 value
lang: LangApi array_metadata_name_Pat array_Declaration array_Pat array_Branch array_Exp metadata metadata2 value
lang = {
  eBase = wsMetadata <| \wsb value ->
    withDummyExpInfo <| Utils.fromOk "EvalUpdate" <| ImpureGoodies.matchBaseValue value
         (EBase wsb << EBool)
         (EBase wsb  << EString "\"")
         (\n -> EConst wsb n dummyLoc noWidgetDecl),
  eVar = wsMetadata <| \wsb name ->
    withDummyExpInfo <| EVar wsb name,
  eFun = wsMetadata <| \wsb arraypats body ->
    withDummyExpInfo <| EFun wsb (fromArray arraypats) body space0,
  eApp = wsMetadata <| \wsb e1 e2s ->
    withDummyExpInfo <| EApp wsb e1 (fromArray e2s) SpaceApp space0,
  eOp = wsMetadata <| \wsb opStr e2s ->
    let op = withDummyRange <| Utils.fromJust_ ("The operator " ++ opStr ++ " is not supported") <| Parser.opFromIdentifier opStr in
    withDummyExpInfo <| EOp wsb space1 op (fromArray e2s) space0,
  eList = wsMetadata <| \wsb wsExps ->
    withDummyExpInfo <| EList wsb (wsExps |> fromArray |> List.map ((,) space0)) space0 Nothing space0,
  eCons = wsMetadata <| \wsb head tail ->
    withDummyExpInfo <| EList wsb [(space0, head)] space1 (Just tail) space0,
  eIf = wsMetadata <| \wsb cond thn els ->
    withDummyExpInfo <| EIf wsb cond space1 thn space1 els space0,
  eCase = wsMetadata <| \wsb input branches ->
    withDummyExpInfo <| ECase wsb input (fromArray branches) space0,
  eLet = wsMetadata <| \wsb declarations body ->
    withDummyExpInfo <| ELet wsb Let declarations space1 body,
  eParens = wsMetadata <| \wsb exp style ->
    withDummyExpInfo <| EParens wsb exp (if style == "" then Parens else CustomSyntax style) space0,
  eRecord = wsMetadata <| \wsb declarations ->
    withDummyExpInfo <| ERecord wsb Nothing declarations space0,
  eRecordExtend = wsMetadata <| \wsb previous declarations ->
      withDummyExpInfo <| ERecord wsb (Just (previous, space1)) declarations space0,
  eSelect = wsMetadata <| \wsb expr name ->
    withDummyExpInfo <| ESelect wsb expr space0 space0 name,

  letTypeAlias = wsMetadata <| \wsb pat t ->
     LetType Nothing wsb Nothing pat FunArgAsPats space1 t,

  letTypeData = wsMetadata <| \wsb pat t ->
     LetType Nothing wsb (Just space1) pat FunArgAsPats space1 t,

  letAnnotation = wsMetadata <| \wsb pat t ->
     LetAnnotation Nothing wsb pat FunArgAsPats space1 t,

  letExp = wsMetadata <| \wsb pat exp ->
    LetExp Nothing wsb pat FunArgAsPats space1 exp,
  declType = DeclType,
  declExp = DeclExp,
  declAnnotation = DeclAnnotation,

  declarations = wsMetadata <| \wsb declarationList ->
        case Parser.reorderDeclarations <| fromArray declarationList of
          Err msg -> Debug.crash msg
          Ok declarations -> declarations,

  {-declarations = wsMetadata <| \wsb po letTypes letAnns letExps ->
    let printOrder =
          if List.length po == 0 then
            List.range 0 (lengthGroup letTypes + List.length letAnns + lengthGroup letExps - 1)
          else po
    in
    Declarations printOrder letTypes letAnns letExps,-}
  branch = wsMetadata <| \wsb pat exp ->
    withDummyRange <| Branch_ wsb pat exp space1,
  pVar = wsMetadata <| \wsb name ->
    withDummyPatInfo <| PVar wsb name noWidgetDecl,
  pBase = wsMetadata <| \wsb value ->
    withDummyPatInfo <| Utils.fromOk "EvalUpdate" <| ImpureGoodies.matchBaseValue value
    (PBase wsb << EBool)
    (PBase wsb  << EString "\"")
    (\n -> PConst wsb n),
  pWildcard = wsMetadata <| (withDummyPatInfo << PWildcard),
  pList = wsMetadata <| \wsb wsPats ->
     withDummyPatInfo <| PList wsb (fromArray wsPats) space0 Nothing space0,
  pCons = wsMetadata <| \wsb head tail ->
     withDummyPatInfo <| PList wsb [head] space1 (Just tail) space0,
  pRecord = wsMetadata <| \wsb pats ->
     withDummyPatInfo <| PRecord wsb (pats |> fromArray |> List.map (\{metadata,name,pat} ->
       (Just space0, ws <| whitespaceFromMetadata metadata, name, space1, pat)
     )) space0,
  pParens = wsMetadata <| \wsb pat ->
     withDummyPatInfo <| PParens wsb pat space0,
  pAs = wsMetadata <| \wsb pat1 pat2 ->
     withDummyPatInfo <| PAs wsb pat1 space1 pat2,
  unapply = {
    eBase = \exp -> case unwrapExp exp of
      EConst ws n _ _    -> Just (mdOf ws, ImpureGoodies.hideType n)
      EBase ws (EBool b) -> Just (mdOf ws, ImpureGoodies.hideType b)
      EBase ws (EString q content) ->
                            Just (mdOf ws, ImpureGoodies.hideType content)
      _ -> Nothing,
    eVar = \exp -> case unwrapExp exp of
      EVar ws name -> Just (mdOf ws, name)
      _ -> Nothing,
    eFun = \exp -> case unwrapExp exp of
      EFun wsb pats body _ -> Just (mdOf wsb, toArray pats, body)
      _ -> Nothing,
    eApp = \exp -> case unwrapExp exp of
      EApp wsb e1 e2s _ _ -> Just (mdOf wsb, e1, toArray e2s)
      _ -> Nothing,
    eOp = \exp -> case unwrapExp exp of
      EOp wsb _ op e2s _ -> Just (mdOf wsb, LeoUnparser.unparseOp op, toArray e2s)
      _ -> Nothing,
    eList = \exp -> case unwrapExp exp of
      EList wsb elems _ Nothing _ ->
        Just (mdOf wsb, List.map Tuple.second elems |> toArray)
      _ -> Nothing,
    eCons = \exp -> case unwrapExp exp of
      EList wsb [(_, head)] _ (Just tail) _ ->
        Just (mdOf wsb, head, tail)
      _ -> Nothing,
    eIf = \exp -> case unwrapExp exp of
      EIf wsb cond _ thn _ els _ ->
        Just (mdOf wsb, cond, thn ,els)
      _ -> Nothing,
    eCase = \exp -> case unwrapExp exp of
      ECase wsb input branches _ ->
        Just (mdOf wsb, input, toArray branches)
      _ -> Nothing,
    eLet = \exp -> case unwrapExp exp of
      ELet wsb _ declarations _ body ->
        Just (mdOf wsb, declarations, body)
      _ -> Nothing,
    eParens = \exp -> case unwrapExp exp of
      EParens wsb sub (CustomSyntax syntax) _ -> Just (mdOf wsb, sub, syntax)
      EParens wsb sub _ _ -> Just (mdOf wsb, sub, "")
      _ -> Nothing,
    eRecord = \exp -> case unwrapExp exp of
      ERecord wsb Nothing declarations _ ->
        Just (mdOf wsb, declarations)
      _ -> Nothing,
    eRecordExtend = \exp -> case unwrapExp exp of
      ERecord wsb (Just (init, _)) declarations _ ->
        Just (mdOf wsb, init, declarations)
      _ -> Nothing,
    eSelect = \exp -> case unwrapExp exp of
      ESelect wsb expr _ _ name -> Just (mdOf wsb, exp, name)
      _ -> Nothing,
    letTypeAlias = \letType -> case letType of
       LetType Nothing wsb Nothing pat _ _ t ->
         Just (mdOf wsb, pat, t)
       _ -> Nothing,
    letTypeData = \letType -> case letType of
      LetType Nothing wsb (Just _) pat _ _ t ->
         Just (mdOf wsb, pat, t)
      _ -> Nothing,
    letAnnotation = \(LetAnnotation _ wsb pat _ _ t) -> 
      (mdOf wsb, pat, t),
    letExp = \(LetExp _ wsb pat _ _ exp) ->
      (mdOf wsb, pat, exp),
    declType = \d -> case d of
      DeclType d -> Just d
      _ -> Nothing,
    declAnnotation = \d -> case d of
      DeclAnnotation d -> Just d
      _ -> Nothing,
    declExp = \d -> case d of
      DeclExp d -> Just d
      _ -> Nothing,
    declarations = \declarations ->
      getDeclarationsInOrder declarations |> toArray,
    --declarations: Declarations -> (metadata, PrintOrder, GroupsOf LetType, List LetAnnotation, GroupsOf LetExp),
    branch = \b -> case b.val of
      Branch_ wsb pat exp _ -> (mdOf wsb, pat, exp),
    pVar = \p -> case p.val.p__ of
      PVar wsb name _ -> Just (mdOf wsb, name)
      _ -> Nothing,
    pBase = \p -> case p.val.p__ of
      PBase wsb (EBool b) -> Just (mdOf wsb, ImpureGoodies.hideType b)
      PBase wsb (EString _ s) -> Just (mdOf wsb, ImpureGoodies.hideType s)
      PConst wsb n -> Just (mdOf wsb, ImpureGoodies.hideType n)
      _ -> Nothing,
    pWildcard = \p -> case p.val.p__ of
      PWildcard wsb -> Just (mdOf wsb)
      _ -> Nothing,
    pList = \p -> case p.val.p__ of
      PList wsb pats _ Nothing _ ->
        Just (mdOf wsb, toArray pats)
      _ -> Nothing,
    pCons = \p -> case p.val.p__ of
      PList wsb [head] _ (Just tail) _ ->
        Just (mdOf wsb, head, tail)
      _ -> Nothing,
    pRecord = \p -> case p.val.p__ of
      PRecord wsb pats _ ->
        Just (mdOf wsb, pats |> List.map (\(_, wsm, name, _, pat) -> {metadata=mdOf wsm, name=name, pat=pat}) |> toArray)
      _ -> Nothing,
    pParens = \p -> case p.val.p__ of
      PParens wsb pat _ ->
        Just (mdOf wsb, pat)
      _ -> Nothing,
    pAs = \p -> case p.val.p__ of
      PAs wsb pat1 _ pat2 ->
        Just (mdOf wsb, pat1, pat2)
      _ -> Nothing
  }
  }

api: {
  parse: String -> Result String Exp,
  evaluate: Exp -> Result String (Val, Env),
  update: Exp -> (Val, Env)-> Val -> Results String Exp,
  evaluateWithoutCache: Exp -> Result String Val,
  updateWithoutCache: Exp -> Val -> Results String Exp,
  lazyList: LazyListType a,
  unparse: Exp -> String,
  process: Result err a -> (a -> Result err b) -> Result err b,
  valToNative: Val -> Result String nativea,
  nativeToVal: nativeb -> Val,
  valToString: Val -> String,
  valToHTMLSource: Val -> Result String String,
  string: StringType envA envB envC,
  objEnv: ObjEnvType envA envB envC,
  fromOk: Result String a -> a,
  foldResult: (err -> b) -> (a -> b) -> Result err a -> b,
  first: (a, b) -> a,
  second: (a, b) -> b,
  lang: LangApi array_metadata_name_Pat array_Declaration array_Pat array_Branch array_Exp metadata metadata2 value
  }
api = {
  -- parse: String -> Result String Exp
  parse = parse,
  -- evaluate: Exp -> Result String Val
  evaluate = evalExp,
  -- updateExp: Exp -> Val -> Val -> Result String (LazyList Exp)
  update = updateExp,
  lazyList = lazyList,
  evaluateWithoutCache = evalExpWithoutCache,
  updateWithoutCache = updateExpWithoutCache,
  -- unparse: Exp -> String
  unparse = unparse,
  -- process: Result err a -> (a -> Result err b) -> Result err b,
  process = flip Result.andThen,
  -- valToNative: Val -> JSAny
  valToNative = valToNative, -- Converts a Val to native Javascript (except closures)
  -- nativeToVal: JSAny -> Val
  nativeToVal = nativeToVal (builtinVal "EvalUpdate.nativeToVal"),
  -- valToString: Val -> String
  valToString = LangUtils.valToString,
  -- Val -> Result String String
  valToHTMLSource = LangSvg.valToHTMLSource HTMLParser.HTML,
  string = string,
  objEnv = objEnv,
  lang = lang,
  fromOk = \res -> case res of
    Ok x -> x
    Err msg -> Debug.crash msg,
  foldResult = \errHandler okHandler res -> case res of
    Ok x -> okHandler x
    Err msg -> errHandler msg,
  first = Tuple.first,
  second = Tuple.second
  }