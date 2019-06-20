--------------------------------------------------------------------------------
-- This module contains all the declarations for the theory, as well as all the
-- helper functions (parsing, unparsing, conversion, mapping, etc.) necessary
-- for working with them.
--------------------------------------------------------------------------------

module UnLang exposing
  ( ..
  )

import Dict exposing (Dict)
import Char

import Parser as P exposing (Parser, (|=), (|.))
import Parser.LanguageKit as LanguageKit
import ParserUtils exposing (..)

import State exposing (State)

import Lang exposing (Exp, Ident, HoleId, Num)
import LeoUnparser

import Pos exposing (Pos, startPos, posFromRowCol)
import Info exposing (WithInfo, withInfo)

import Utils

--==============================================================================
--= Declarations
--==============================================================================

--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------

type alias FunctionDefinition =
  -- (Name, Param, Body)
  (Ident, Ident, Exp)

type EnvBinding
  = VarBinding Ident (UnExp ())
  | RecursiveBinding Env (List FunctionDefinition)

type alias Env =
  List EnvBinding

--------------------------------------------------------------------------------
-- Partial Functions
--------------------------------------------------------------------------------

type alias PartialFunction =
  List
    ( UnExp () -- arg
    , Example -- output
    )

--------------------------------------------------------------------------------
-- UnExps
--------------------------------------------------------------------------------

type alias HoleIndex =
  (HoleId, Int)

-- The d is for extra data
type UnExp d
  = UConstructor d Ident (UnExp d)
  | UTuple d (List (UnExp d))
  | UPartialFunction d PartialFunction
  | UFunClosure d Env Ident Exp
  | UHoleClosure d Env HoleIndex
  | UApp d (UnExp d) (UnExp d)
  | UGet d Int Int (UnExp d)
  | UConstructorInverse  d Ident (UnExp d)
  | UCase d Env (UnExp d) (List (Ident, Ident, Exp))

--------------------------------------------------------------------------------
-- UnVals
--------------------------------------------------------------------------------

type UnVal
  = UVConstructor Ident UnVal
  | UVTuple (List UnVal)
  | UVPartialFunction PartialFunction
  | UVFunClosure Env Ident Exp

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

type Example
  = ExConstructor Ident Example
  | ExTuple (List Example)
  | ExPartialFunction PartialFunction
  | ExDontCare

--------------------------------------------------------------------------------
-- Worlds
--------------------------------------------------------------------------------

type alias World =
  (Env, Example)

type alias Worlds =
  List World

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

type alias Constraint =
  (HoleId, World)

type alias Constraints =
  List Constraint

--------------------------------------------------------------------------------
-- Hole Fillings
--------------------------------------------------------------------------------

type alias HoleFilling =
  Dict HoleId Exp

--==============================================================================
--= Environment Functions
--==============================================================================

pairsToEnv : List (Ident, UnExp ()) -> Env
pairsToEnv =
  List.map (Utils.uncurry VarBinding)

addVarBinding : Ident -> UnExp () -> Env -> Env
addVarBinding i u env =
  VarBinding i u :: env

addRecursiveBinding : (Env, List FunctionDefinition) -> Env -> Env
addRecursiveBinding (functionEnv, functionDefs) env =
  RecursiveBinding functionEnv functionDefs :: env

envVarBindings : Env -> List (Ident, UnExp ())
envVarBindings =
  List.concatMap <| \binding ->
    case binding of
      VarBinding i u ->
        [(i, u)]

      _ ->
        []

envRecursiveBindings : Env -> List (Env, List FunctionDefinition)
envRecursiveBindings =
  List.concatMap <| \binding ->
    case binding of
      RecursiveBinding functionEnv functionDefs ->
        [(functionEnv, functionDefs)]

      _ ->
        []

lookupVar : Ident -> Env -> Maybe (UnExp ())
lookupVar x =
  envVarBindings >> Utils.maybeFind x

lookupRecursiveFunction :
  Ident -> Env -> Maybe (Env, List FunctionDefinition, FunctionDefinition)
lookupRecursiveFunction functionName =
  let
    extract (functionEnv, functionDefs) =
      functionDefs
        |> Utils.maybeFindBy (\(name, _, _) -> name == functionName)
        |> Maybe.map (\fd -> (functionEnv, functionDefs, fd))
  in
    envRecursiveBindings >> Utils.mapFirstSuccess extract

--==============================================================================
--= Unparsing
--==============================================================================

unparseEnv : Env -> String
unparseEnv =
  let
    showFunction : FunctionDefinition -> String
    showFunction (name, param, body) =
      name
        ++ " "
        ++ param
        ++ " = "
        ++ LeoUnparser.unparse body

    showBinding : EnvBinding -> String
    showBinding binding =
      case binding of
        VarBinding ident u ->
          ident ++ " → " ++ unparseSimple u

        RecursiveBinding functionEnv functions ->
          String.concat
            [ "<"
            , functions
                |> List.map showFunction
                |> String.join " ; "
            , ">"
            ]
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

    eatString : String -> State UnparseState String
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
        State.map (\_ -> s) <|
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

    newline : State UnparseState String
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
            let
              (extraOpen, extraClose) =
                case uArg of
                  UTuple _ _ ->
                    (" ", "")

                  _ ->
                    (" (", ")")
            in
              State.do (eatString <| name ++ extraOpen) <| \_ ->
              State.do (unparseHelper uArg) <| \uArgWithInfo ->
              State.do (eatString extraClose) <| \_ ->
              State.do State.get <| \end ->
              State.pure <|
                let
                  argInfo =
                    getData uArgWithInfo
                in
                  UConstructor
                    ( withInfo
                        ( name
                            ++ extraOpen
                            ++ argInfo.val
                            ++ extraClose
                        )
                        start.pos
                        end.pos
                    )
                    name
                    uArgWithInfo

          UTuple _ us ->
            if List.isEmpty us then
              State.do (eatString "()") <| \_ ->
              State.do State.get <| \end ->
              State.pure <|
                UTuple (withInfo "()" start.pos end.pos) []
            else if shouldBreak us then
              let
                entry u =
                  State.do indent <| \_ ->
                  State.do (unparseHelper u) <| \uWithInfo ->
                  State.do dedent <| \_ ->
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

          UPartialFunction _ pf ->
            basic "<partial function>" (\w -> UPartialFunction w pf)

          UFunClosure _ env param body ->
            let
              unparsedString =
                "["
                  ++ unparseEnv env
                  ++ "] λ"
                  ++ param
                  ++ " ."
                  ++ LeoUnparser.unparse body
            in
              basic
                unparsedString
                (\w -> UFunClosure w env param body)

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

          UApp _ uFunction uArg ->
            if shouldBreak [uArg] then
              let
                entry u =
                  State.do newline <| \_ ->
                  -- State.do (eatString "(") <| \_ ->
                  State.do (unparseHelper u) <| \uWithInfo ->
                  -- State.do (eatString ")") <| \_ ->
                  State.pure
                    uWithInfo
              in
                State.do (unparseHelper uFunction) <| \uFunctionWithInfo ->
                State.do indent <| \_ ->
                State.do (entry uArg) <| \uArgWithInfo ->
                State.do dedent <| \_ ->
                State.do newline <| \_ ->
                State.do State.get <| \end ->
                State.pure <|
                  let
                    argString =
                      indentString (start.indent + 1)
                        ++ (getData uArgWithInfo).val
                  in
                    UApp
                      ( withInfo
                          ( (getData uFunctionWithInfo).val
                              -- Provides area to select application
                              ++ " <|"
                              ++ argString
                              ++ indentString (start.indent + 1))
                          start.pos
                          end.pos
                      )
                      uFunctionWithInfo
                      uArgWithInfo
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
                State.do (entry uArg) <| \uArgWithInfo ->
                State.do State.get <| \end ->
                State.pure <|
                  let
                    argString =
                      " (" ++ (getData uArgWithInfo).val ++ ")"
                  in
                    UApp
                      ( withInfo
                          ( (getData uFunctionWithInfo).val
                              ++ argString
                          )
                          start.pos
                          end.pos
                      )
                      uFunctionWithInfo
                      uArgWithInfo

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

          UConstructorInverse d ident uArg ->
            unparseHelper <|
              UConstructor d (ident ++ "^-1") uArg

          UCase _ env uScrutinee branches ->
            let
              unparsedEnv =
                "[" ++ unparseEnv env ++ "]"

              unparseBranch (ctorName, argName, body) =
                let
                  matchLine =
                    if String.startsWith "__" argName then
                      ctorName ++ " →"
                    else
                      ctorName ++ " " ++ argName ++ " →"
                in
                  State.do State.get <| \start ->
                  State.do (eatString matchLine) <| \_ ->
                  State.do indent <| \_ ->
                  State.do newline <| \_ ->
                  -- LeoUnparser.unparse body
                  State.do (eatString "...") <| \_ ->
                  State.do dedent <| \_ ->
                  State.do newline <| \_ ->
                  State.do newline <| \_ ->
                  State.pure <|
                    matchLine
                      ++ indentString (start.indent + 1)
                      ++ "...\n"
                      ++ indentString start.indent
            in
              State.do (eatString unparsedEnv) <| \_ ->
              State.do newline <| \_ ->
              State.do (eatString "case ") <| \_ ->
              State.do (unparseHelper uScrutinee) <| \uScrutineeWithInfo ->
              State.do (eatString " of") <| \_ ->
              State.do indent <| \_ ->
              State.do newline <| \_ ->
              State.do (State.mapM unparseBranch branches) <| \branchStrings ->
              State.do dedent <| \_ ->
              State.do State.get <| \end ->
              State.pure <|
                UCase
                  ( withInfo
                      ( unparsedEnv
                          ++ indentString start.indent
                          ++ "case "
                          ++ (getData uScrutineeWithInfo).val
                          ++ " of"
                          ++ indentString (start.indent + 1)
                          ++ String.join "" branchStrings
                      )
                      start.pos
                      end.pos
                  )
                  env
                  uScrutineeWithInfo
                  branches
  in
    unparseHelper >> State.run { pos = startPos, indent = 0 } >> Tuple.first

unparseSimple : UnExp d -> String
unparseSimple =
  unparse >> getData >> .val

--==============================================================================
--= Generic UnExp Functions
--==============================================================================

getData : UnExp d -> d
getData u =
  case u of
    UConstructor d _ _ ->
      d

    UTuple d _ ->
      d

    UPartialFunction d _ ->
      d

    UFunClosure d _ _ _ ->
      d

    UHoleClosure d _ _ ->
      d

    UApp d _ _ ->
      d

    UGet d _ _ _ ->
      d

    UConstructorInverse d _ _ ->
      d

    UCase d _ _ _ ->
      d

mapData : (d -> e) -> UnExp d -> UnExp e
mapData f u =
  case u of
    UConstructor d ident arg ->
      UConstructor (f d) ident (mapData f arg)

    UTuple d args ->
      UTuple (f d) (List.map (mapData f) args)

    UPartialFunction d pf ->
      UPartialFunction (f d) pf

    UFunClosure d env params body ->
      UFunClosure (f d) env params body

    UHoleClosure d env holeIndex ->
      UHoleClosure (f d) env holeIndex

    UApp d uFunction uArg ->
      UApp (f d) (mapData f uFunction) (mapData f uArg)

    UGet d n i uTuple ->
      UGet (f d) n i (mapData f uTuple)

    UConstructorInverse d ident arg ->
      UConstructorInverse (f d) ident (mapData f arg)

    UCase d env uScrutinee branches ->
      UCase (f d) env (mapData f uScrutinee) branches

statefulMap : (UnExp d -> State s (UnExp d)) -> UnExp d -> State s (UnExp d)
statefulMap f u =
  flip State.andThen (f u) <| \uNew ->
    case uNew of
      UConstructor d ident arg ->
        State.map (UConstructor d ident) (statefulMap f arg)

      UTuple d args ->
        State.map (UTuple d) (State.mapM (statefulMap f) args)

      UPartialFunction d pf ->
        State.pure <| UPartialFunction d pf

      UFunClosure d env params body ->
        State.pure <| UFunClosure d env params body

      UHoleClosure d env holeIndex ->
        State.pure <| UHoleClosure d env holeIndex

      UApp d uFunction uArg ->
        flip State.andThen (statefulMap f uFunction) <| \newFunction ->
          State.map (UApp d newFunction) (statefulMap f uArg)

      UGet d n i uTuple ->
        State.map (UGet d n i) (statefulMap f uTuple)

      UConstructorInverse d ident arg ->
        State.map (UConstructorInverse d ident) (statefulMap f arg)

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

    UTuple _ args ->
      args

    UPartialFunction _ _ ->
      []

    UFunClosure _ _ _ _ ->
      []

    UHoleClosure _ _ _ ->
      []

    UApp _ uFunction uArg ->
      [uFunction, uArg]

    UGet _ _ _ uTuple ->
      [uTuple]

    UConstructorInverse _ _ arg ->
      [arg]

    UCase _ _ uScrutinee _ ->
      [uScrutinee]

flatten : UnExp d -> List (UnExp d)
flatten u =
  u :: List.concatMap flatten (children u)

--==============================================================================
--= UnExp Helper Functions
--==============================================================================

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

--==============================================================================
--= UnExp / UnVal / Example Conversion
--==============================================================================

expToVal : UnExp d -> Maybe UnVal
expToVal u =
  case u of
    UConstructor _ ident arg ->
      Maybe.map (UVConstructor ident) (expToVal arg)

    UTuple _ args ->
      args
        |> List.map expToVal
        |> Utils.projJusts
        |> Maybe.map UVTuple

    UPartialFunction _ pf ->
      Just <|
        UVPartialFunction pf

    UFunClosure _ env params body ->
      Just <|
        UVFunClosure env params body

    UHoleClosure _ _ _ ->
      Nothing

    UApp _ _ _ ->
      Nothing

    UGet _ _ _ _ ->
      Nothing

    UConstructorInverse _ _ _ ->
      Nothing

    UCase _ _ _ _ ->
      Nothing


valToExp : UnVal -> UnExp ()
valToExp v =
  case v of
    UVConstructor ident arg ->
      UConstructor () ident (valToExp arg)

    UVTuple args ->
      UTuple () (List.map valToExp args)

    UVPartialFunction pf ->
      UPartialFunction () pf

    UVFunClosure env params body ->
      UFunClosure () env params body

valToExample : UnVal -> Maybe Example
valToExample v =
  case v of
    UVConstructor ident arg ->
      Maybe.map (ExConstructor ident) (valToExample arg)

    UVTuple args ->
      args
        |> List.map valToExample
        |> Utils.projJusts
        |> Maybe.map ExTuple

    UVPartialFunction pf ->
      Just <|
        ExPartialFunction pf

    UVFunClosure env params body ->
      Nothing

exampleToVal : Example -> Maybe UnVal
exampleToVal ex =
  case ex of
    ExConstructor ident arg ->
      Maybe.map (UVConstructor ident) (exampleToVal arg)

    ExTuple args ->
      args
        |> List.map exampleToVal
        |> Utils.projJusts
        |> Maybe.map UVTuple

    ExPartialFunction pf ->
      Just <|
        UVPartialFunction pf

    ExDontCare ->
      Nothing

expToExample : UnExp d -> Maybe Example
expToExample =
  expToVal >> Maybe.andThen valToExample

exampleToExp : Example -> Maybe (UnExp ())
exampleToExp =
  exampleToVal >> Maybe.map valToExp

--==============================================================================
--= Parsing
--==============================================================================

--------------------------------------------------------------------------------
-- Generic
--------------------------------------------------------------------------------

spaces : Parser ()
spaces =
  P.ignore P.zeroOrMore (\char -> char == ' ' || char == '\n' || char == '\t')

capitalIdentifier : Parser String
capitalIdentifier =
  P.succeed (++)
    |= P.keep (P.Exactly 1) Char.isUpper
    |= P.keep P.zeroOrMore (\c -> Char.isUpper c || Char.isLower c)

--------------------------------------------------------------------------------
-- UnVals
--------------------------------------------------------------------------------

uvConstructor : Parser UnVal
uvConstructor =
  P.lazy <| \_ ->
    P.inContext "constructor" <|
      P.succeed UVConstructor
        --( \ctorName maybeArg ->
        --    maybeArg
        --      |> Maybe.withDefault (UVTuple [])
        --      |> UVConstructor ctorName
        --)
        |= capitalIdentifier
        |. spaces
        -- Syntactic sugar for applying to unit
        -- |= optional unval
        |= unval

uvNum : Parser UnVal
uvNum =
  P.inContext "number" <|
    let
      nonNegativeInt : Parser Int
      nonNegativeInt =
        P.int
          |> P.andThen
               ( \n ->
                   if n < 0 then
                     P.fail "Negative integer examples not supported"
                   else
                      P.succeed n
               )

      buildNum : Int -> UnVal
      buildNum n =
        Utils.iterate
          n
          (UVConstructor "S")
          (UVConstructor "Z" (UVTuple []))
    in
      P.map buildNum nonNegativeInt

-- uvBool : Parser UnVal
-- uvBool =
--   P.inContext "boolean" <|
--     P.oneOf
--       [ ParserUtils.token "True" <|
--           UVConstructor "T" (UVTuple [])
--       , ParserUtils.token "False" <|
--           UVConstructor "F" (UVTuple [])
--       ]

-- Parses 1-tuples as just the element it contains (handles "parenthesized
-- expressions")
uvTuple : Parser UnVal
uvTuple =
  P.lazy <| \_ ->
    P.inContext "tuple" <|
      P.map
        ( \uArgs ->
            case uArgs of
              [uArg] ->
                uArg

              _ ->
                UVTuple uArgs
        )
        ( LanguageKit.sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = unval
            , trailing = LanguageKit.Forbidden
            }
        )

uvList : Parser UnVal
uvList =
  P.lazy <| \_ ->
    P.inContext "list" <|
      P.map
        ( List.foldr
            ( \element list ->
                UVConstructor "Cons" (UVTuple [element, list])
            )
            ( UVConstructor "Nil" (UVTuple [])
            )
        )
        ( LanguageKit.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = spaces
            , item = unval
            , trailing = LanguageKit.Forbidden
            }
        )

uvPartialFunction : Parser UnVal
uvPartialFunction =
  P.lazy <| \_ ->
    let
      binding : Parser (UnExp (), Example)
      binding =
        P.succeed (,)
          |= P.map valToExp unval
          |. spaces
          |. P.symbol "->"
          |. spaces
          |= example
    in
      P.inContext "partial function example" <|
        P.map UVPartialFunction <|
          LanguageKit.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = spaces
            , item = binding
            , trailing = LanguageKit.Forbidden
            }

uvFunClosure : Parser UnVal
uvFunClosure =
  P.fail "function closure unval not yet supported"

unval : Parser UnVal
unval =
  P.lazy <| \_ ->
    P.succeed identity
      |. spaces
      |= P.oneOf
           [ uvNum
           , uvTuple
           , uvList -- syntactic sugar for constructors
           , uvPartialFunction
           , uvConstructor
           ]

parseUnval : String -> Result P.Error UnVal
parseUnval =
  P.run unval

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

example : Parser Example
example =
  P.lazy <| \_ ->
    P.map valToExample unval |> P.andThen (\maybeEx ->
      case maybeEx of
        Just ex ->
          P.succeed ex

        Nothing ->
          P.fail "could not convert from unval to example"
    )

parseExample : String -> Result P.Error Example
parseExample =
  P.run example
