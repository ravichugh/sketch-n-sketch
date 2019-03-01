module UnExp exposing
  ( Env
  , UnExp(..)
  , UnVal(..)
  , getData
  , asExp, asValue
  , unval
  , unparseEnv, unparse, unparseSimple
  , mapData, statefulMap, map, children, flatten
  , findHoles, findHoleEId
  )

import Char

import State exposing (State)

import Parser as P exposing (..)
import Parser.LanguageKit as LanguageKit
import ParserUtils exposing (..)

import Lang exposing (Exp, Ident, HoleId, Num)
import LeoUnparser

import Pos exposing (Pos, startPos, posFromRowCol)
import Info exposing (WithInfo, withInfo)

import Utils

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type alias HoleIndex =
  (HoleId, Int)

type alias Env =
  List (Ident, UnExp ())

-- The d is for extra data
type UnExp d
  = UConstructor d Ident (UnExp d)
  | UNum d Num
  | UBool d Bool
  | UString d String
  | UTuple d (List (UnExp d))
  | UFunClosure d Env (List Ident) {- Type -} Exp
  | UHoleClosure d Env HoleIndex
  | UApp d (UnExp d) (List (UnExp d))
  | UGet d Int Int (UnExp d)
  | UCase d Env (UnExp d) (List (Ident, Ident, Exp))

type UnVal
  = UVConstructor Ident UnVal
  | UVNum Num
  | UVBool Bool
  | UVString String
  | UVTuple (List UnVal)
  | UVFunClosure Env (List Ident) {- Type -} Exp

--------------------------------------------------------------------------------
-- Extra Data
--------------------------------------------------------------------------------

getData : UnExp d -> d
getData u =
  case u of
    UConstructor d _ _ ->
      d

    UNum d _ ->
      d

    UBool d _ ->
      d

    UString d _ ->
      d

    UTuple d _ ->
      d

    UFunClosure d _ _ _ ->
      d

    UHoleClosure d _ _ ->
      d

    UApp d _ _ ->
      d

    UGet d _ _ _ ->
      d

    UCase d _ _ _ ->
      d

--------------------------------------------------------------------------------
-- Value Conversion
--------------------------------------------------------------------------------

asExp : UnVal -> UnExp ()
asExp v =
  case v of
    UVConstructor ident arg ->
      UConstructor () ident (asExp arg)

    UVNum n ->
      UNum () n

    UVBool b ->
      UBool () b

    UVString s ->
      UString () s

    UVTuple args ->
      UTuple () (List.map asExp args)

    UVFunClosure env params body ->
      UFunClosure () env params body

asValue : UnExp d -> Maybe UnVal
asValue u =
  case u of
    UConstructor _ ident arg ->
      Maybe.map (UVConstructor ident) (asValue arg)

    UNum _ n ->
      Just <|
        UVNum n

    UBool _ b ->
      Just <|
        UVBool b

    UString _ s ->
      Just <|
        UVString s

    UTuple _ args ->
      args
        |> List.map asValue
        |> Utils.projJusts
        |> Maybe.map UVTuple

    UFunClosure _ env params body ->
      Just <|
        UVFunClosure env params body

    UHoleClosure _ _ _ ->
      Nothing

    UApp _ _ _ ->
      Nothing

    UGet _ _ _ _ ->
      Nothing

    UCase _ _ _ _ ->
      Nothing

--------------------------------------------------------------------------------
-- Value Parsing
--------------------------------------------------------------------------------

spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')

capitalIdentifier : Parser String
capitalIdentifier =
  succeed (++)
    |= keep (Exactly 1) Char.isUpper
    |= keep zeroOrMore (\c -> Char.isUpper c || Char.isLower c)

uvConstructor : Parser UnVal
uvConstructor =
  lazy <| \_ ->
    inContext "constructor unval" <|
      succeed UVConstructor
        |= capitalIdentifier
        |= unval

uvNum : Parser UnVal
uvNum =
  let
    sign =
      oneOf
        [ succeed (-1)
            |. symbol "-"
        , succeed 1
        ]
  in
    try <|
      inContext "number unval" <|
        succeed (\s n -> UVNum (s * n))
          |= sign
          |= float

uvBool : Parser UnVal
uvBool =
  inContext "boolean unval" <|
    P.map UVBool <|
      oneOf
        [ token "True" True
        , token "False" False
        ]

uvString : Parser UnVal
uvString =
  inContext "string unval" <|
    P.map (\(_, content) -> UVString content)
      singleLineString

uvTuple : Parser UnVal
uvTuple =
  lazy <| \_ ->
    inContext "tuple unval" <|
      P.map UVTuple <|
        LanguageKit.sequence
          { start = "("
          , separator = ","
          , end = ")"
          , spaces = spaces
          , item = unval
          , trailing = LanguageKit.Forbidden
          }

uvFunClosure : Parser UnVal
uvFunClosure =
  fail "function closure unval not yet supported"

unval : Parser UnVal
unval =
  lazy <| \_ ->
    oneOf
       [ uvNum
       , uvBool
       , uvString
       , uvTuple
       , uvConstructor
       ]

parseVal : String -> Result P.Error UnVal
parseVal =
  run unval

--------------------------------------------------------------------------------
-- Unparsing
--------------------------------------------------------------------------------

unparseEnv : Env -> String
unparseEnv =
  let
    showBinding : (Ident, UnExp d) -> String
    showBinding (i, u) =
      i ++ " → " ++ unparseSimple u
  in
    List.map showBinding >> String.join ", "

type alias UnparseState =
  { pos : Pos
  , indent : Int
  }

unparse : UnExp d -> UnExp (WithInfo String)
unparse =
  let
    shouldBreak : List (UnExp d) -> Bool
    shouldBreak us =
      let
        strings =
          List.map unparseSimple us

        maxArgLen =
          strings
            |> List.map (String.length)
            |> List.maximum
            |> Maybe.withDefault 0

        containsNewline =
          List.any (String.contains "\n") strings
      in
        maxArgLen > 20 || containsNewline

    eatString : String -> State UnparseState ()
    eatString s =
      let
        lines =
          String.split "\n" s

        newLineCount =
          List.length lines - 1

        lastLineLength =
          -- String.split always returns a non-empty list
          lines
            |> Utils.last_
            |> String.length
      in
        if newLineCount > 0 then
          State.modify
            ( \state ->
                { state
                    | pos =
                        { line = state.pos.line + newLineCount
                        , col = lastLineLength + 1
                        }
                }
            )
        else
          State.modify
            ( \state ->
                { state
                    | pos =
                        { line = state.pos.line
                        , col = state.pos.col + lastLineLength
                        }
                }
            )

    basic :
      String
        -> ((WithInfo String)
        -> UnExp (WithInfo String))
        -> State UnparseState (UnExp (WithInfo String))
    basic s uFunc =
      State.do State.get <| \start ->
      State.do (eatString s) <| \_ ->
      State.do State.get <| \end ->
      State.pure <|
        uFunc (withInfo s start.pos end.pos)

    newline : State UnparseState ()
    newline =
      State.do State.get <| \state ->
      eatString <|
        indentString state.indent

    indentString : Int -> String
    indentString n =
      "\n" ++ String.concat (List.repeat n "  ")

    indent : State UnparseState ()
    indent =
      State.modify <| \state ->
        { state | indent = state.indent + 1 }

    dedent : State UnparseState ()
    dedent =
      State.modify <| \state ->
        { state | indent = state.indent - 1 }

    unparseHelper : UnExp d -> State UnparseState (UnExp (WithInfo String))
    unparseHelper u =
      State.do State.get <| \start ->
        case u of
          UConstructor _ name uArg ->
            State.do (eatString <| name ++ " ") <| \_ ->
            State.do (unparseHelper uArg) <| \uArgWithInfo ->
            State.pure <|
              let
                argInfo =
                  getData uArgWithInfo
              in
                UConstructor
                  ( withInfo
                      (name ++ " " ++ argInfo.val)
                      start.pos
                      argInfo.end
                  )
                  name
                  uArgWithInfo

          UNum _ n ->
            let
              nString =
                toString n
            in
              basic nString (flip UNum n)

          UBool _ b ->
            let
              bString =
                if b then "True" else "False"
            in
              basic bString (flip UBool b)

          UString _ s ->
            let
              sString =
                "\"" ++ s ++ "\""
            in
              basic sString (flip UString s)

          UTuple _ us ->
            if shouldBreak us then
              let
                entry u =
                  State.do (unparseHelper u) <| \uWithInfo ->
                  State.do newline <| \_ ->
                  State.do (eatString ", ") <| \_ ->
                  State.pure <|
                    uWithInfo
              in
                State.do (eatString "( ") <| \_ ->
                State.do (State.mapM entry us) <| \usWithInfo ->
                State.do (eatString ")") <| \_ ->
                -- Un-eat final comma+space
                State.do
                  ( State.modify <| \state ->
                      { state
                          | pos =
                              { line = state.pos.line
                              , col = state.pos.col - 2
                              }
                      }
                  ) <| \_ ->
                State.do State.get <| \end ->
                State.pure <|
                  let
                    innerString =
                      usWithInfo
                        |> List.map (getData >> .val)
                        |> String.join (indentString start.indent ++ ", ")
                  in
                    UTuple
                      ( withInfo
                        ( "( "
                            ++ innerString
                            ++ indentString start.indent
                            ++ ")"
                        )
                        start.pos
                        end.pos
                      )
                      usWithInfo
            else
              let
                entry u =
                  State.do (unparseHelper u) <| \uWithInfo ->
                  State.do (eatString ", ") <| \_ ->
                  State.pure <|
                    uWithInfo
              in
                State.do (eatString "(") <| \_ ->
                State.do (State.mapM entry us) <| \usWithInfo ->
                State.do (eatString ")") <| \_ ->
                -- Un-eat final comma+space
                State.do
                  ( State.modify <| \state ->
                      { state
                          | pos =
                              { line = state.pos.line
                              , col = state.pos.col - 2
                              }
                      }
                  ) <| \_ ->
                State.do State.get <| \end ->
                State.pure <|
                  let
                    innerString =
                      usWithInfo
                        |> List.map (getData >> .val)
                        |> String.join ", "
                  in
                    UTuple
                      (withInfo ("(" ++ innerString ++ ")") start.pos end.pos)
                      usWithInfo

          UFunClosure _ env args body ->
            let
              argsString =
                String.join ", " args

              unparsedString =
                "["
                  ++ unparseEnv env
                  ++ "] λ"
                  ++ argsString
                  ++ " ."
                  ++ LeoUnparser.unparse body
            in
              basic unparsedString (\w -> UFunClosure w env args body)

          UHoleClosure _ env (i, j) ->
            let
              unparsedString =
                "["
                  ++ unparseEnv env
                  ++ "] ??("
                  ++ toString i
                  ++ ", "
                  ++ toString j
                  ++ ")"
            in
              basic unparsedString (\w -> UHoleClosure w env (i, j))

          UApp _ uFunction uArgs ->
            if shouldBreak uArgs then
              let
                entry u =
                  State.do newline <| \_ ->
                  State.do (eatString "(") <| \_ ->
                  State.do (unparseHelper u) <| \uWithInfo ->
                  State.do (eatString ")") <| \_ ->
                  State.pure
                    uWithInfo
              in
                State.do (unparseHelper uFunction) <| \uFunctionWithInfo ->
                State.do indent <| \_ ->
                State.do (State.mapM entry uArgs) <| \uArgsWithInfo ->
                State.do dedent <| \_ ->
                State.do newline <| \_ ->
                State.do State.get <| \end ->
                State.pure <|
                  let
                    argString =
                      uArgsWithInfo
                        |> List.map
                             ( \u ->
                                 indentString (start.indent + 1)
                                   ++ "("
                                   ++ (getData u).val
                                   ++ ")"
                             )
                        |> String.concat
                  in
                    UApp
                      ( withInfo
                          ( (getData uFunctionWithInfo).val
                              ++ argString
                              ++ indentString (start.indent + 1))
                          start.pos
                          end.pos
                      )
                      uFunctionWithInfo
                      uArgsWithInfo
            else
              let
                entry u =
                  State.do (eatString " (") <| \_ ->
                  State.do (unparseHelper u) <| \uWithInfo ->
                  State.do (eatString ")") <| \_ ->
                  State.pure
                    uWithInfo
              in
                State.do (unparseHelper uFunction) <| \uFunctionWithInfo ->
                State.do (State.mapM entry uArgs) <| \uArgsWithInfo ->
                State.do State.get <| \end ->
                State.pure <|
                  let
                    argString =
                      uArgsWithInfo
                        |> List.map (\u -> " (" ++ (getData u).val ++ ")")
                        |> String.concat
                  in
                    UApp
                      ( withInfo
                          ((getData uFunctionWithInfo).val ++ argString)
                          start.pos
                          end.pos
                      )
                      uFunctionWithInfo
                      uArgsWithInfo

          UGet _ n i uTuple ->
            let
              funcName =
                "get_" ++ toString n ++ "_" ++ toString i
            in
              State.do (eatString <| funcName ++ " ") <| \_ ->
              State.do (unparseHelper uTuple) <| \uTupleWithInfo ->
              State.pure <|
                let
                  tupleInfo =
                    getData uTupleWithInfo
                in
                  UGet
                    ( withInfo
                        (funcName ++ " " ++ tupleInfo.val)
                        start.pos
                        tupleInfo.end
                    )
                    n
                    i
                    uTupleWithInfo

          UCase _ env u0 branches ->
            -- TODO
            Debug.crash "Case not supported"
--            let
--              unparseBranch (constructorName, varName, body) =
--                constructorName
--                  ++ " "
--                  ++ varName ++ " →"
--                  ++ LeoUnparser.unparse body
--            in
--              "["
--                ++ unparseEnv env
--                ++ "] case "
--                ++ unparse u0
--                ++ " of "
--                ++ String.join " " (List.map unparseBranch branches)
  in
    unparseHelper >> State.run { pos = startPos, indent = 0 } >> Tuple.first

unparseSimple : UnExp d -> String
unparseSimple =
  unparse >> getData >> .val

--------------------------------------------------------------------------------
-- Generic Library
--------------------------------------------------------------------------------

mapData : (d -> e) -> UnExp d -> UnExp e
mapData f u =
  case u of
    UConstructor d ident arg ->
      UConstructor (f d) ident (mapData f arg)

    UNum d n ->
      UNum (f d) n

    UBool d b ->
      UBool (f d) b

    UString d s ->
      UString (f d) s

    UTuple d args ->
      UTuple (f d) (List.map (mapData f) args)

    UFunClosure d env params body ->
      UFunClosure (f d) env params body

    UHoleClosure d env holeIndex ->
      UHoleClosure (f d) env holeIndex

    UApp d uFunction uArgs ->
      UApp (f d) (mapData f uFunction) (List.map (mapData f) uArgs)

    UGet d n i uTuple ->
      UGet (f d) n i (mapData f uTuple)

    UCase d env uScrutinee branches ->
      UCase (f d) env (mapData f uScrutinee) branches

statefulMap : (UnExp d -> State s (UnExp d)) -> UnExp d -> State s (UnExp d)
statefulMap f u =
  flip State.andThen (f u) <| \uNew ->
    case uNew of
      UConstructor d ident arg ->
        State.map (UConstructor d ident) (statefulMap f arg)

      UNum d n ->
        State.pure <| UNum d n

      UBool d b ->
        State.pure <| UBool d b

      UString d s ->
        State.pure <| UString d s

      UTuple d args ->
        State.map (UTuple d) (State.mapM (statefulMap f) args)

      UFunClosure d env params body ->
        State.pure <| UFunClosure d env params body

      UHoleClosure d env holeIndex ->
        State.pure <| UHoleClosure d env holeIndex

      UApp d uFunction uArgs ->
        flip State.andThen (statefulMap f uFunction) <| \newFunction ->
          State.map (UApp d newFunction) (State.mapM (statefulMap f) uArgs)

      UGet d n i uTuple ->
        State.map (UGet d n i) (statefulMap f uTuple)

      UCase d env uScrutinee branches ->
        State.map
          (\newScrutinee -> UCase d env newScrutinee branches)
          (statefulMap f uScrutinee)

map : (UnExp d -> UnExp d) -> UnExp d -> UnExp d
map f =
  statefulMap (f >> State.pure) >> State.run () >> Tuple.first

children : UnExp d -> List (UnExp d)
children u =
  case u of
    UConstructor _ _ arg ->
      [arg]

    UNum _ _ ->
      []

    UBool _ _ ->
      []

    UString _ _ ->
      []

    UTuple _ args ->
      args

    UFunClosure _ _ _ _ ->
      []

    UHoleClosure _ _ _ ->
      []

    UApp _ uFunction uArgs ->
      uFunction :: uArgs

    UGet _ _ _ uTuple ->
      [uTuple]

    UCase _ _ uScrutinee _ ->
      [uScrutinee]

flatten : UnExp d -> List (UnExp d)
flatten u =
  u :: List.concatMap flatten (children u)

--------------------------------------------------------------------------------
-- Additional Functions
--------------------------------------------------------------------------------

findHoles : HoleId -> UnExp d -> List (Int, Env)
findHoles targetHoleId =
  let
    extract u =
      case u of
        UHoleClosure d env (holeId, index) ->
          if holeId == targetHoleId then
            [(index, env)]
          else
            []

        _ ->
          []
  in
    flatten
      >> List.concatMap extract
      >> List.sortBy Tuple.first

findHoleEId : Exp -> UnExp d -> Maybe Lang.EId
findHoleEId ast u =
  let
    holeMatches idToMatch e =
      case Lang.unwrapExp e of
        Lang.EHole _ (Lang.EEmptyHole id) ->
          id == idToMatch

        _ ->
          False
  in
    case u of
      UHoleClosure _ _ (i, _) ->
        ast
          |> Lang.findFirstNode (holeMatches i)
          |> Maybe.map Lang.expEId

      _ ->
        Nothing
