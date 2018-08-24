module FastParser exposing
  ( prelude, isPreludeLoc, isPreludeLocId, isPreludeEId, isActualEId, isProgramEId
  , substOf, substStrOf, substPlusOf
  , parse
  , parseE, parseT
  , sanitizeVariableName
  , clearAllIds
  , freshen
  , maxId
  )

import List
import Char

import String exposing (fromChar)
import Set exposing (Set)
import Dict exposing (Dict)

import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Parser.LowLevel exposing (getOffset, getSource)

import ParserUtils exposing (..)
import LangParserUtils exposing (..)

import Utils as U
import PreludeGenerated as Prelude
import Lang exposing (..)
import TopLevelExp exposing (TopLevelExp, fuseTopLevelExps)
import Pos exposing (..)
import Info exposing (..)

import ImpureGoodies
import LangUnparser
import Utils

spaces = oldSpaces

--==============================================================================
--= PARSER WITH INFO
--==============================================================================

type alias ParserI a = Parser (WithInfo a)

--==============================================================================
--= HELPERS
--==============================================================================

--------------------------------------------------------------------------------
-- Block Helper
--------------------------------------------------------------------------------

block
  : (WS -> a -> WS -> b) -> String -> String -> Parser a -> ParserI b
block combiner openSymbol closeSymbol p =
  delayedCommitMap
    ( \(wsStart, open) (result, wsEnd, close) ->
        WithInfo
          (combiner wsStart result wsEnd)
          open.start
          close.end
    )
    ( succeed (,)
        |= spaces
        |= trackInfo (symbol openSymbol)
    )
    ( succeed (,,)
        |= p
        |= spaces
        |= trackInfo (symbol closeSymbol)
    )

parenBlock : (WS -> a -> WS -> b) -> Parser a -> ParserI b
parenBlock combiner = block combiner "(" ")"

bracketBlock : (WS -> a -> WS -> b) -> Parser a -> ParserI b
bracketBlock combiner = block combiner "[" "]"

blockIgnoreWS : String -> String -> Parser a -> ParserI a
blockIgnoreWS = block (\wsStart x wsEnd -> x)

parenBlockIgnoreWS : Parser a -> ParserI a
parenBlockIgnoreWS = blockIgnoreWS "(" ")"

--------------------------------------------------------------------------------
-- List Helper
--------------------------------------------------------------------------------

listLiteralInternal
  :  String
  -> (WS -> List (WithInfo elem) -> WS -> list)
  -> ParserI elem -> ParserI list
listLiteralInternal context combiner elem =
  inContext context <|
    bracketBlock combiner (repeat zeroOrMore elem)

multiConsInternal
  :  String
  -> (WS -> List (WithInfo elem) -> WS -> (WithInfo elem) -> WS -> list)
  -> ParserI elem
  -> ParserI list
multiConsInternal context combiner elem =
  inContext context <|
    bracketBlock
      ( \wsStart (heads, wsBar, tail) wsEnd ->
          combiner wsStart heads wsBar tail wsEnd
      )
      ( delayedCommitMap
          ( \(heads, wsBar) tail ->
              (heads, wsBar, tail)
          )
          ( succeed (,)
              |= repeat oneOrMore elem
              |= spaces
              |. symbol "|"
          )
          elem
      )

genericList
  :  { generalContext
         : String
     , listLiteralContext
         : String
     , multiConsContext
         : String
     , listLiteralCombiner
         : WS -> List (WithInfo elem) -> WS -> list
     , multiConsCombiner
         : WS -> List (WithInfo elem) -> WS -> (WithInfo elem) -> WS -> list
     , elem
         : ParserI elem
     }
  -> ParserI list
genericList args =
  inContext args.generalContext <|
    oneOf
      [ multiConsInternal
          args.multiConsContext
          args.multiConsCombiner
          args.elem
      , listLiteralInternal
          args.listLiteralContext
          args.listLiteralCombiner
          args.elem
      ]

--==============================================================================
--= NUMBERS
--==============================================================================

--------------------------------------------------------------------------------
-- Raw Numbers
--------------------------------------------------------------------------------

num : ParserI Num
num =
  let
    sign =
      oneOf
        [ succeed (-1)
            |. symbol "-"
        , succeed 1
        ]
  in
    try <|
      inContext "number" <|
        trackInfo <|
          succeed (\s n -> s * n)
            |= sign
            |= float

--------------------------------------------------------------------------------
-- Widget Declarations
--------------------------------------------------------------------------------

isInt : Num -> Bool
isInt n = n == toFloat (floor n)

widgetDecl : Caption -> Parser WidgetDecl
widgetDecl cap =
  let
    combiner a tok b =
      if List.all isInt [a.val, b.val] then
        IntSlider (mapInfo floor a) tok (mapInfo floor b) cap False
      else
        NumSlider a tok b cap False
  in
    inContext "widget declaration" <|
      trackInfo <|
        oneOf
          [ succeed combiner
              |. symbol "{"
              |= num
              |= trackInfo (token "-" "-")
              |= num
              |. symbol "}"
          , succeed NoWidgetDecl
          ]

--------------------------------------------------------------------------------
-- Frozen Annotations
--------------------------------------------------------------------------------

frozenAnnotation : ParserI Frozen
frozenAnnotation =
  inContext "frozen annotation" <|
    trackInfo <|
      oneOf <|
        List.map (\a -> token a a) [frozen, thawed, assignOnlyOnce, unann]

--==============================================================================
--= BASE VALUES
--==============================================================================

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------

string : ParserI EBaseVal
string =
  let
    stringHelper quoteChar =
      let
        quoteString = fromChar quoteChar
      in
        succeed (EString quoteString)
          |. symbol quoteString
          |= keep zeroOrMore (\c -> c /= quoteChar)
          |. symbol quoteString
  in
    inContext "string" <|
      trackInfo <|
        oneOf <| List.map stringHelper ['\'', '"'] -- " fix syntax highlighting

--------------------------------------------------------------------------------
-- Bools
--------------------------------------------------------------------------------

bool : ParserI EBaseVal
bool =
  inContext "bool" <|
    trackInfo <|
      map EBool <|
        oneOf <|
          [ map (always True) <| keyword "true"
          , map (always False) <| keyword "false"
          ]

--------------------------------------------------------------------------------
-- Nulls
--------------------------------------------------------------------------------

null : ParserI EBaseVal
null =
  inContext "null" <|
    trackInfo <|
      map (always ENull) <| keyword "null"

--------------------------------------------------------------------------------
-- General Base Values
--------------------------------------------------------------------------------

baseValue : ParserI EBaseVal
baseValue =
  inContext "base value" <|
    oneOf
      [ string
      , bool
      , null
      ]

--==============================================================================
--= IDENTIFIERS
--==============================================================================

--------------------------------------------------------------------------------
-- General Identifiers
--------------------------------------------------------------------------------

validIdentifierRestChar : Char -> Bool
validIdentifierRestChar c =
  Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_' || c == '\''

keywords : Set String
keywords =
  Set.fromList
    [ "true"
    , "false"
    , "pi"
    , "cos"
    , "sin"
    , "arccos"
    , "arcsin"
    , "floor"
    , "ceiling"
    , "round"
    , "toString"
    , "sqrt"
    , "explode"
    , "mod"
    , "arctan2"
    , "if"
    , "case"
    , "typecase"
    , "let"
    , "letrec"
    , "def"
    , "defrec"
    , "typ"
    , "__DictEmpty__"
    , "__DictInsert__"
    , "__DictGet__"
    , "__DictRemove__"
    , "__DictFromList__"
    , "debug"
    , "noWidgets"
    ]

--------------------------------------------------------------------------------
-- Variable Identifiers
--------------------------------------------------------------------------------

validVariableIdentifierFirstChar : Char -> Bool
validVariableIdentifierFirstChar c =
  Char.isLower c || c == '_'

variableIdentifierString : ParserI Ident
variableIdentifierString =
  inContext "variable identifier string" <|
    trackInfo <|
      variable validVariableIdentifierFirstChar validIdentifierRestChar keywords

--------------------------------------------------------------------------------
-- Type Identifiers
--------------------------------------------------------------------------------

validTypeIdentifierFirstChar : Char -> Bool
validTypeIdentifierFirstChar c =
  Char.isUpper c

typeIdentifierString : ParserI Ident
typeIdentifierString =
  inContext "type identifier string" <|
    trackInfo <|
      variable validTypeIdentifierFirstChar validIdentifierRestChar keywords

--==============================================================================
--= PATTERNS
--==============================================================================

--------------------------------------------------------------------------------
-- Name Pattern Helper
--------------------------------------------------------------------------------

namePattern : ParserI Ident -> Parser Pat
namePattern ident =
  mapPat_ <|
    paddedBefore (\ws name -> PVar ws name noWidgetDecl) spaces ident

--------------------------------------------------------------------------------
-- Variable Pattern
--------------------------------------------------------------------------------

variablePattern : Parser Pat
variablePattern =
  inContext "variable pattern" <|
    namePattern variableIdentifierString

--------------------------------------------------------------------------------
-- Type Pattern (SPECIAL-USE ONLY; not included in `pattern`)
--------------------------------------------------------------------------------

typePattern : Parser Pat
typePattern =
  inContext "type pattern" <|
    namePattern typeIdentifierString

--------------------------------------------------------------------------------
-- Constant Pattern
--------------------------------------------------------------------------------

constantPattern : Parser Pat
constantPattern =
  mapPat_ <|
    inContext "constant pattern" <|
      paddedBefore PConst spaces num

--------------------------------------------------------------------------------
-- Base Value Pattern
--------------------------------------------------------------------------------

baseValuePattern : Parser Pat
baseValuePattern =
  mapPat_ <|
    inContext "base value pattern" <|
      paddedBefore PBase spaces baseValue

--------------------------------------------------------------------------------
-- Pattern Lists
--------------------------------------------------------------------------------

patternList : Parser Pat
patternList =
  mapPat_ <|
    lazy <| \_ ->
      genericList
        { generalContext =
            "pattern list"
        , listLiteralContext =
            "pattern list literal"
        , multiConsContext =
            "pattern multi cons literal"
        , listLiteralCombiner =
            ( \wsStart heads wsEnd ->
                PList wsStart heads space0 Nothing wsEnd
            )
        , multiConsCombiner =
            ( \wsStart heads wsBar tail wsEnd ->
                PList wsStart heads wsBar (Just tail) wsEnd
            )
        , elem =
            pattern
        }

--------------------------------------------------------------------------------
-- As-Patterns (@-Patterns)
--------------------------------------------------------------------------------

asPattern : Parser Pat
asPattern =
  mapPat_ <|
    inContext "as pattern" <|
      lazy <| \_ ->
        delayedCommitMap
          ( \(wsStart, name, wsAt) pat ->
              WithInfo (PAs wsStart (withInfo (pat_ <| PVar space1 name.val noWidgetDecl) name.start name.end) wsAt pat) name.start pat.end
          )
          ( succeed (,,)
              |= spaces
              |= variableIdentifierString
              |= spaces
              |. symbol "@"
          )
          pattern

--------------------------------------------------------------------------------
-- General Patterns
--------------------------------------------------------------------------------

pattern : Parser Pat
pattern =
  inContext "pattern" <|
    oneOf
      [ lazy <| \_ -> patternList
      , lazy <| \_ -> asPattern
      , constantPattern
      , baseValuePattern
      , variablePattern
      ]

--==============================================================================
--= TYPES
--==============================================================================

--------------------------------------------------------------------------------
-- Base Types
--------------------------------------------------------------------------------

baseType : String -> (WS -> Type_) -> String -> Parser Type
baseType context combiner token =
  inContext context <|
    delayedCommitMap
      ( \ws _ ->
          WithInfo (combiner ws) ws.start ws.end
      )
      ( spaces )
      ( keyword token )

nullType : Parser Type
nullType =
  baseType "null type" TNull "Null"

numType : Parser Type
numType =
  baseType "num type" TNum "Num"

boolType : Parser Type
boolType =
  baseType "bool type" TBool "Bool"

stringType : Parser Type
stringType =
  baseType "string type" TString "String"

--------------------------------------------------------------------------------
-- Variable Types
--------------------------------------------------------------------------------

variableType : Parser Type
variableType =
  inContext "variable type" <|
    paddedBefore TVar spaces variableIdentifierString

--------------------------------------------------------------------------------
-- Function Type
--------------------------------------------------------------------------------

functionType : Parser Type
functionType =
  lazy <| \_ ->
    inContext "function type" <|
      parenBlock TArrow <|
        succeed identity
          |. keywordWithSpace "->"
          |= repeat oneOrMore typ

--------------------------------------------------------------------------------
-- List Type
--------------------------------------------------------------------------------

listType : Parser Type
listType =
  inContext "list type" <|
    lazy <| \_ ->
      parenBlock TList <|
        succeed identity
          |. keywordWithSpace "List"
          |= typ

--------------------------------------------------------------------------------
-- Dict Type
--------------------------------------------------------------------------------

dictType : Parser Type
dictType =
  inContext "dictionary type" <|
    lazy <| \_ ->
      parenBlock
        ( \wsStart (tKey, tVal) wsEnd ->
            TDict wsStart tKey tVal wsEnd
        )
        ( succeed (,)
            |. keywordWithSpace "TDict"
            |= typ
            |= typ
        )

--------------------------------------------------------------------------------
-- Tuple Type
--------------------------------------------------------------------------------

tupleType : Parser Type
tupleType =
  lazy <| \_ ->
    genericList
      { generalContext =
          "tuple type"
      , listLiteralContext =
          "tuple type list literal"
      , multiConsContext =
          "tuple type multi cons literal"
      , listLiteralCombiner =
          ( \wsStart heads wsEnd ->
              TTuple wsStart heads space0 Nothing wsEnd
          )
      , multiConsCombiner =
          ( \wsStart heads wsBar tail wsEnd ->
              TTuple wsStart heads wsBar (Just tail) wsEnd
          )
      , elem =
          typ
      }

--------------------------------------------------------------------------------
-- Forall Type
--------------------------------------------------------------------------------

forallType : Parser Type
forallType =
  let
    patVar =
      delayedCommitMap
        ( \ws name ->
            withInfo (TPatVar ws name.val) name.start name.end
        )
        spaces
        variableIdentifierString
    quantifiers = inContext "forall type (one)" <| repeat zeroOrMore patVar
  in
    inContext "forall type" <|
      lazy <| \_ ->
        parenBlock
          ( \wsBefore (qs, t) wsEnd ->
              TForall wsBefore qs t wsEnd
          )
          ( succeed (,)
              |. keywordWithSpace "forall"
              |= quantifiers
              |= typ
          )

--------------------------------------------------------------------------------
-- Union Type
--------------------------------------------------------------------------------

unionType : Parser Type
unionType =
  inContext "union type" <|
    lazy <| \_ ->
      parenBlock TUnion <|
        succeed identity
          |. keywordWithSpace "union"
          |= repeat oneOrMore typ

--------------------------------------------------------------------------------
-- Named Types
--------------------------------------------------------------------------------
-- Full `TApp` types not supported in little syntax

namedType : Parser Type
namedType =
  inContext "named type" <|
    paddedBefore
      ( \wsBefore ident ->
          TVar wsBefore ident
      )
      spaces
      typeIdentifierString


--------------------------------------------------------------------------------
-- Wildcard Type
--------------------------------------------------------------------------------

wildcardType : Parser Type
wildcardType =
  inContext "wildcard type" <|
    spaceSaverKeyword spaces "_" TWildcard

--------------------------------------------------------------------------------
-- General Types
--------------------------------------------------------------------------------

typ : Parser Type
typ =
  inContext "type" <|
    lazy <| \_ ->
      oneOf
        [ nullType
        , numType
        , boolType
        , stringType
        , wildcardType
        , lazy <| \_ -> functionType
        , lazy <| \_ -> listType
        , lazy <| \_ -> dictType
        , lazy <| \_ -> tupleType
        , lazy <| \_ -> forallType
        , lazy <| \_ -> unionType
        , namedType
        , variableType
        ]

--==============================================================================
--= EXPRESSIONS
--==============================================================================

--------------------------------------------------------------------------------
-- Identifier Expressions
--------------------------------------------------------------------------------

variableExpression : ParserI Exp_
variableExpression =
  mapExp_ <|
    paddedBefore EVar spaces variableIdentifierString

--------------------------------------------------------------------------------
-- Constant Expressions
--------------------------------------------------------------------------------

constantExpression : ParserI Exp_
constantExpression =
  mapExp_ <|
    delayedCommitMap
      ( \ws (n, fa, w) ->
          WithInfo
            (EConst ws n.val (dummyLocWithDebugInfo fa.val n.val) w)
            n.start
            w.end
      )
      spaces
      ( succeed (,,)
          |= num
          |= frozenAnnotation
          |= widgetDecl Nothing
      )

--------------------------------------------------------------------------------
-- Base Value Expressions
--------------------------------------------------------------------------------

baseValueExpression : ParserI Exp_
baseValueExpression =
  inContext "base value expression" <|
    mapExp_ <|
      paddedBefore EBase spaces baseValue

--------------------------------------------------------------------------------
-- Primitive Operators
--------------------------------------------------------------------------------

operator : ParserI Exp_
operator =
  mapExp_ <|
    let
      op =
        trackInfo <|
          oneOf
            [ -- Unary Operators
              succeed Pi
                |. keyword "pi"
            , succeed DictEmpty
                |. keyword "__DictEmpty__"
            , succeed CurrentEnv
                |. keyword "__CurrentEnv__"
              -- Non-unary operators
            , succeed DictFromList
                |. keyword "__DictFromList__"
            , succeed Cos
                |. keywordWithSpace "cos"
            , succeed Sin
                |. keywordWithSpace "sin"
            , succeed ArcCos
                |. keywordWithSpace "arccos"
            , succeed ArcSin
                |. keywordWithSpace "arcsin"
            , succeed Floor
                |. keywordWithSpace "floor"
            , succeed Ceil
                |. keywordWithSpace "ceiling"
            , succeed Round
                |. keywordWithSpace "round"
            , succeed ToStr
                |. keywordWithSpace "toString"
            , succeed Sqrt
                |. keywordWithSpace "sqrt"
            , succeed Explode
                |. keywordWithSpace "explode"
            , succeed Plus
                |. keywordWithSpace "+"
            , succeed Minus
                |. keywordWithSpace "-"
            , succeed Mult
                |. keywordWithSpace "*"
            , succeed Div
                |. keywordWithSpace "/"
            , succeed Lt
                |. keywordWithSpace "<"
            , succeed Eq
                |. keywordWithSpace "="
            , succeed Mod
                |. keywordWithSpace "mod"
            , succeed Pow
                |. keywordWithSpace "^"
            , succeed ArcTan2
                |. keywordWithSpace "arctan2"
            , succeed DictInsert
                |. keywordWithSpace "__DictInsert__"
            , succeed DictGet
                |. keywordWithSpace "__DictGet__"
            , succeed DictRemove
                |. keywordWithSpace "__DictRemove__"
            , succeed DebugLog
                |. keywordWithSpace "debug"
            , succeed NoWidgets
                |. keywordWithSpace "noWidgets"
            ]
    in
      inContext "operator" <|
        lazy <| \_ ->
          parenBlock
            ( \wsStart (opName, args) wsEnd ->
                EOp wsStart space0 opName (List.map Expr args) wsEnd
            )
            ( succeed (,)
                |= op
                |= repeat zeroOrMore exp
            )

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : ParserI Exp_
conditional =
  mapExp_ <|
    inContext "conditional" <|
      lazy <| \_ ->
        parenBlock
          ( \wsStart (c, a, b) wsEnd ->
              EIf wsStart (Expr c) (ws "") (Expr a) (ws "") (Expr b) wsEnd
          )
          ( succeed (,,)
             |. keywordWithSpace "if"
             |= exp
             |= exp
             |= exp
          )

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

list : ParserI Exp_
list =
  mapExp_ <|
    lazy <| \_ ->
      genericList
        { generalContext =
            "list"
        , listLiteralContext =
            "list literal"
        , multiConsContext =
            "multi cons literal"
        , listLiteralCombiner =
            ( \wsStart heads wsEnd ->
                EList wsStart (List.map (\e_ -> (space0, Expr e_)) heads) space0 Nothing wsEnd
            )
        , multiConsCombiner =
            ( \wsStart heads wsBar tail wsEnd ->
                EList wsStart (List.map (\e_ -> (space0, Expr e_)) heads) wsBar (Just <| Expr tail) wsEnd
            )
        , elem =
            exp
        }

--------------------------------------------------------------------------------
-- Branch Helper
--------------------------------------------------------------------------------

genericCase
  :  String
  -> String
  -> (WS -> a -> (List (WithInfo branch)) -> WS -> Exp__)
  -> (WS -> b -> Exp -> WS -> branch)
  -> Parser a
  -> Parser b
  -> ParserI Exp_
genericCase context kword combiner branchCombiner parser branchParser =
  let
    path =
      inContext (context ++ " path") <|
        lazy <| \_ ->
          parenBlock
            ( \wsStart (p, e) wsEnd ->
                branchCombiner wsStart p (Expr e) wsEnd
            )
            ( succeed (,)
                |= branchParser
                |= exp
            )
  in
    mapExp_ <|
      inContext context <|
        lazy <| \_ ->
          parenBlock
            ( \wsStart (c, branches) wsEnd ->
                combiner wsStart c branches wsEnd
            )
            ( succeed (,)
                |. keywordWithSpace kword
                |= parser
                |= repeat zeroOrMore path
            )

--------------------------------------------------------------------------------
-- Case Expressions
--------------------------------------------------------------------------------

caseExpression : ParserI Exp_
caseExpression =
    lazy <| \_ ->
      genericCase
        "case expression" "case"
        ECase Branch_ (map Expr exp) pattern

--------------------------------------------------------------------------------
-- Type Case Expressions
--------------------------------------------------------------------------------

typeCaseExpression : ParserI Exp_
typeCaseExpression =
    lazy <| \_ ->
      genericCase
        "type case expression" "typecase"
        ECase (\wsStart tp e wsEnd ->
          Branch_ space1 (withDummyPatInfo <| PColonType space0 (withDummyPatInfo <| PWildcard space0) space1 tp) e space1
        ) (map Expr exp) typ

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

function : ParserI Exp_
function =
  let
    parameters =
      oneOf
        [ map List.singleton pattern
        , untrackInfo <| parenBlockIgnoreWS <| repeat oneOrMore pattern
        ]
  in
    mapExp_ <|
      inContext "function" <|
        lazy <| \_ ->
          parenBlock
            ( \wsStart (params, body) wsEnd ->
                EFun wsStart params (Expr body) wsEnd
            )
            ( succeed (,)
                |. symbol "\\"
                |= parameters
                |= exp
            )

--------------------------------------------------------------------------------
-- Function Applications
--------------------------------------------------------------------------------

functionApplication : ParserI Exp_
functionApplication =
  mapExp_ <|
    inContext "function application" <|
      lazy <| \_ ->
        parenBlock
          ( \wsStart (f, x) wsEnd ->
              EApp wsStart (Expr f) (List.map Expr x) SpaceApp wsEnd
          )
          ( succeed (,)
              |= exp
              |= repeat oneOrMore exp
          )

--------------------------------------------------------------------------------
-- Let Bindings
--------------------------------------------------------------------------------

genericLetBinding : String -> String -> Bool -> ParserI Exp_
genericLetBinding context kword isRec =
  mapExp_ <|
    inContext context <|
      parenBlock
        ( \wsStart (name, binding, rest) wsEnd ->
            eLet__ wsStart Let isRec name space1 (Expr binding) space1 (Expr rest) wsEnd
        )
        ( succeed (,,)
            |. keywordWithSpace kword
            |= pattern
            |= exp
            |= exp
        )

genericDefBinding : String -> String -> Bool -> ParserI Exp_
genericDefBinding context kword isRec =
  mapExp_ <|
    inContext context <|
      delayedCommitMap
        ( \(wsStart, open) (name, binding, wsEnd, close, rest) ->
            WithInfo
              (eLet__ wsStart Def isRec name space1 (Expr binding) space1 (Expr rest) wsEnd)
              open.start
              close.end
        )
        ( succeed (,)
            |= spaces
            |= trackInfo (symbol "(")
        )
        ( succeed (,,,,)
            |. keywordWithSpace kword
            |= pattern
            |= exp
            |= spaces
            |= trackInfo (symbol ")")
            |= exp
        )

recursiveLetBinding : ParserI Exp_
recursiveLetBinding =
  lazy <| \_ ->
    genericLetBinding "recursive let binding" "letrec" True

simpleLetBinding : ParserI Exp_
simpleLetBinding =
  lazy <| \_ ->
    genericLetBinding "non-recursive let binding" "let" False

recursiveDefBinding : ParserI Exp_
recursiveDefBinding =
  lazy <| \_ ->
    genericDefBinding "recursive def binding" "defrec" True

simpleDefBinding : ParserI Exp_
simpleDefBinding =
  lazy <| \_ ->
    genericDefBinding "non-recursive def binding" "def" False

letBinding : ParserI Exp_
letBinding =
  inContext "let binding" <|
    lazy <| \_ ->
      oneOf
        [ recursiveLetBinding
        , simpleLetBinding
        , recursiveDefBinding
        , simpleDefBinding
        ]

--------------------------------------------------------------------------------
-- Holes
--------------------------------------------------------------------------------

hole : ParserI Exp_
hole =
  inContext "hole" <|
    mapExp_ <|
      paddedBefore EHole spaces (trackInfo <| token "??" EEmptyHole)

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

typeDeclaration : ParserI Exp_
typeDeclaration =
  mapExp_ <|
    inContext "type declaration" <|
      delayedCommitMap
        ( \(wsStart, open) (pat, t, wsEnd, close, rest) ->
            WithInfo
              (eTyp_ wsStart pat t (Expr rest) wsEnd)
              open.start
              close.end
        )
        ( succeed (,)
            |= spaces
            |= trackInfo (symbol "(")
        )
        ( succeed (,,,,)
            |. keywordWithSpace "typ"
            |= variablePattern
            |= typ
            |= spaces
            |= trackInfo (symbol ")")
            |= exp
        )

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

typeAlias : ParserI Exp_
typeAlias =
  mapExp_ <|
    inContext "type alias" <|
      delayedCommitMap
        ( \(wsStart, open, pat) (t, wsEnd, close, rest) ->
            WithInfo
              (eTypeAlias__ wsStart pat t (Expr rest) wsEnd)
              open.start
              close.end
        )
        ( succeed (,,)
            |= spaces
            |= trackInfo (symbol "(")
            |. keywordWithSpace "def "
            |= typePattern
        )
        ( succeed (,,,)
            |= typ
            |= spaces
            |= trackInfo (symbol ")")
            |= exp
        )

--------------------------------------------------------------------------------
-- Type Annotations
--------------------------------------------------------------------------------

typeAnnotation : ParserI Exp_
typeAnnotation =
  mapExp_ <|
    inContext "type annotation" <|
      lazy <| \_ ->
        parenBlock
          ( \wsStart (e, wsColon, t) wsEnd ->
              EColonType wsStart (Expr e) wsColon t wsEnd
          )
          ( delayedCommitMap
              ( \(e, wsColon) t ->
                  (e, wsColon, t)
              )
              ( succeed (,)
                  |= exp
                  |= spaces
                  |. symbol ":"
              )
              typ
          )

--------------------------------------------------------------------------------
-- General Expressions
--------------------------------------------------------------------------------

exp : ParserI Exp_
exp =
  inContext "expression" <|
    lazy <| \_ ->
      oneOf
        [ constantExpression
        , baseValueExpression
        , lazy <| \_ -> typeAlias
        , lazy <| \_ -> conditional
        , lazy <| \_ -> letBinding
        , lazy <| \_ -> caseExpression
        , lazy <| \_ -> typeCaseExpression
        , lazy <| \_ -> typeDeclaration
        , lazy <| \_ -> typeAnnotation
        , lazy <| \_ -> list
        , lazy <| \_ -> function
        , lazy <| \_ -> functionApplication
        , lazy <| \_ -> operator
        , lazy <| \_ -> hole
        , variableExpression
        ]

--==============================================================================
--= TOP-LEVEL EXPRESSIONS
--==============================================================================

--------------------------------------------------------------------------------
-- Top-Level Defs
--------------------------------------------------------------------------------

genericTopLevelDef : String -> String -> Bool -> Parser TopLevelExp
genericTopLevelDef context kword isRec =
  inContext context <|
    parenBlock
      ( \wsStart (name, binding) wsEnd ->
          ( \rest ->
              exp_ <| eLet__ wsStart Def isRec name space1 (Expr binding) space1 rest wsEnd
          )
      )
      ( succeed (,)
          |. keywordWithSpace kword
          |= pattern
          |= exp
      )

recursiveTopLevelDef : Parser TopLevelExp
recursiveTopLevelDef =
  genericTopLevelDef "top-level recursive def binding" "defrec" True

simpleTopLevelDef : Parser TopLevelExp
simpleTopLevelDef =
  genericTopLevelDef "top-level non-recursive def binding" "def" False

topLevelDef : Parser TopLevelExp
topLevelDef =
  inContext "top-level def binding" <|
    oneOf
      [ recursiveTopLevelDef
      , simpleTopLevelDef
      ]

--------------------------------------------------------------------------------
-- Top-Level Type Declarations
--------------------------------------------------------------------------------

topLevelTypeDeclaration : Parser TopLevelExp
topLevelTypeDeclaration =
  inContext "top-level type declaration" <|
    parenBlock
      ( \wsStart (pat, t) wsEnd ->
          ( \rest ->
              exp_ <| eTyp_ wsStart pat t rest wsEnd
          )
      )
      ( succeed (,)
          |. keywordWithSpace "typ"
          |= variablePattern
          |= typ
      )

--------------------------------------------------------------------------------
-- Top Level Type Aliases
--------------------------------------------------------------------------------

topLevelTypeAlias : Parser TopLevelExp
topLevelTypeAlias =
  inContext "top-level type alias" <|
    delayedCommitMap
      ( \(wsStart, open, pat) (t, wsEnd, close) ->
          WithInfo
            (\rest -> (exp_ <| eTypeAlias__ wsStart pat t rest wsEnd))
            open.start
            close.end
      )
      ( succeed (,,)
          |= spaces
          |= trackInfo (symbol "(")
          |. keywordWithSpace "def"
          |= typePattern
      )
      ( succeed (,,)
          |= typ
          |= spaces
          |= trackInfo (symbol ")")
      )

--------------------------------------------------------------------------------
-- General Top-Level Expressions
--------------------------------------------------------------------------------

topLevelExp : Parser TopLevelExp
topLevelExp =
  inContext "top-level expression" <|
    oneOf
      [ topLevelTypeAlias
      , topLevelDef
      , topLevelTypeDeclaration
      ]

allTopLevelExps : Parser (List TopLevelExp)
allTopLevelExps =
  repeat zeroOrMore topLevelExp

--==============================================================================
--= PROGRAMS
--==============================================================================

implicitMain : ParserI Exp_
implicitMain =
  let
    builder : Pos -> WithInfo Exp_
    builder p =
      let
        withCorrectInfo x =
          WithInfo x p p
        name =
          withCorrectInfo << pat_ <|
            PVar space1 "_IMPLICIT_MAIN" (withDummyInfo NoWidgetDecl)
        binding =
          Expr <|
            withCorrectInfo << exp_ <|
              EBase space1 (EString defaultQuoteChar "...")
        body =
          Expr <|
            withCorrectInfo << exp_ <|
              EVar space1 "main"
      in
        withCorrectInfo << exp_ <|
          ELet newline2 Let (Declarations [0] [] [] [(False, [LetExp Nothing space1 name FunArgAsPats space1 binding])]) space1 body
  in
    succeed builder
      |= getPos

mainExp : ParserI Exp_
mainExp =
  oneOf [exp, implicitMain]

program : ParserI Exp_
program =
  succeed fuseTopLevelExps
    |= allTopLevelExps
    |= mainExp
    |. spaces
    |. end

--==============================================================================
--= EXPORTS
--==============================================================================

--------------------------------------------------------------------------------
-- Parser Runners
--------------------------------------------------------------------------------

parseE_ : (Exp -> Exp) -> String -> Result Error Exp
parseE_ f = run (map (f << Expr) program)

parseE : String -> Result Error Exp
parseE = parseE_ freshen
-- parseE s =
--   ImpureGoodies.logTimedRun "FastParser.parseE" (\() ->
--     parseE_ freshen s
--   )

parse = parseE
  -- so that FastParser and ElmParser (eventually) have same interface

parseT : String -> Result Error Type
parseT = run typ

--------------------------------------------------------------------------------
-- Code from old parser
--------------------------------------------------------------------------------

-- Cousin of variableIdentifierString.
-- Removes invalid characters.
sanitizeVariableName : String -> String
sanitizeVariableName unsafeName =
  unsafeName
  |> String.toList
  |> U.dropWhile (not << (Char.toLower >> validVariableIdentifierFirstChar))
  |> U.mapHead Char.toLower
  |> U.changeTail (List.filter validIdentifierRestChar)
  |> String.fromList

(prelude, initK) = (withDummyExpInfo <| EConst space0 0 dummyLoc noWidgetDecl, 1)
  --freshenClean 1 <| U.fromOkay "parse prelude" <| parseE_ identity "0"

preludeIds = allIds prelude

isPreludeLoc : Loc -> Bool
isPreludeLoc (k,_,_) = isPreludeLocId k

isPreludeLocId : LocId -> Bool
isPreludeLocId k = k < initK

isPreludeEId : EId -> Bool
isPreludeEId k = k < initK

isActualEId : EId -> Bool
isActualEId eid = eid >= 0

isProgramEId : EId -> Bool
isProgramEId eid = isActualEId eid && not (isPreludeEId eid)

clearAllIds : Exp -> Exp
clearAllIds root =
  mapExp clearNodeIds root

-- assign EId's and locId's
-- existing unique EId's/locId's are preserved
-- duplicated and dummy EId's/locId's are reassigned

freshen : Exp -> Exp
freshen e =
  -- let _ = Debug.log ("To Freshen:\n" ++ LangUnparser.unparseWithIds e) () in
  let (duplicateIds, allIds) = duplicateAndAllIds e in
  -- let _ = Debug.log "Duplicate Ids" duplicateIds in
  -- let _ = Debug.log "All Ids" allIds in
  let idsToPreserve = Set.diff allIds duplicateIds in
  -- let _ = Debug.log "Ids to preserve" idsToPreserve in
  let startK      = (List.maximum (initK :: Set.toList allIds) |> U.fromJust_ "freshen") + 1 in
  let (result, _) = freshenPreserving idsToPreserve startK e in
  -- let _ = Debug.log ("Freshened result:\n" ++ LangUnparser.unparseWithIds result) () in
  result

-- Overwrite any existing EId's/locId's
freshenClean : Int -> Exp -> (Exp, Int)
freshenClean initK e = freshenPreserving Set.empty initK e

-- Reassign any id not in idsToPreserve
freshenPreserving : Set.Set Int -> Int -> Exp -> (Exp, Int)
freshenPreserving idsToPreserve initK e =
  let getId k =
    if Set.member k idsToPreserve
    then getId (k+1)
    else k
  in
  let assignIds (Expr exp_) k =
    let exp = Expr exp_ in
    let e__ = unwrapExp exp in
    let (newE__, newK) =
      case e__ of
        EConst ws n (locId, frozen, ident) wd ->
          if Set.member locId idsToPreserve then
            (e__, k)
          else
            let locId = getId k in
            (EConst ws n (locId, frozen, ident) wd, locId + 1)

        ELet ws1 kind (Declarations po tpes ann exps) wsIn body ->
          let (newRevTpes, newK) = Utils.foldLeft ([], k) (elemsOf tpes) <|
            \(revAcc, k) (LetType sp1 spType mbSpAlias pat funPolicy spEq tp) ->
               let (newPat, newK) = freshenPatPreserving idsToPreserve (pat, k) in
               (LetType sp1 spType mbSpAlias newPat funPolicy spEq tp :: revAcc, newK)
          in
          let (newRevAnn, newK2) = Utils.foldLeft ([], newK) ann <|
            \(revAcc, k) (LetAnnotation sp1 sp0 pat funPolicy spEq e1) ->
               let (newPat, newK) = freshenPatPreserving idsToPreserve (pat, k) in
               (LetAnnotation sp1 sp0 newPat funPolicy spEq e1::revAcc, newK)
          in
          let (newRevExps, newK3) = Utils.foldLeft ([], newK2) (elemsOf exps) <|
            \(revAcc, k)  (LetExp sp1 sp0 p funPolicy spEq e1) ->
                let (newP, newK) = freshenPatPreserving idsToPreserve (p, k) in
                let newE1 = recordIdentifiers (newP, e1) in
                (LetExp sp1 sp0 newP funPolicy spEq newE1 :: revAcc, newK)
          in
          (ELet ws1 kind (Declarations po
            (List.reverse newRevTpes |> regroup tpes)
            (List.reverse newRevAnn)
            (List.reverse newRevExps |> regroup exps)) wsIn body, newK3)

        EFun ws1 pats body ws2 ->
          let (newPats, newK) = freshenPatsPreserving idsToPreserve k pats in
          (EFun ws1 newPats body ws2, newK)

        ECase ws1 scrutinee branches ws2 ->
          let (newBranches, newK) =
            branches
            |> List.foldl
                (\branch (newBranches, k) ->
                  let (Branch_ bws1 pat ei bws2) = branch.val in
                  let (newPi, newK) = freshenPatPreserving idsToPreserve (pat, k) in
                  (newBranches ++ [{ branch | val = Branch_ bws1 newPi ei bws2 }], newK)
                )
                ([], k)
          in
          (ECase ws1 scrutinee newBranches ws2, newK)

        _ ->
          (e__, k)
    in
    if Set.member (expEId exp) idsToPreserve then
      (replaceE__ exp newE__, newK)
    else
      let eid = getId newK in
      (Expr <| WithInfo (Exp_ newE__ eid) exp_.start exp_.end, eid + 1)
  in
  mapFoldExp assignIds initK e


-- Reassign any id not in idsToPreserve
freshenPatsPreserving : Set.Set Int -> Int -> List Pat -> (List Pat, Int)
freshenPatsPreserving idsToPreserve initK pats =
  pats
  |> List.foldl
      (\pat (finalPats, k) ->
        let (newPat, newK) = freshenPatPreserving idsToPreserve (pat, k) in
        (finalPats ++ [newPat], newK)
      )
      ([], initK)


-- Reassign any id not in idsToPreserve
freshenPatPreserving : Set.Set Int -> (Pat, Int) -> (Pat, Int)
freshenPatPreserving idsToPreserve (p, initK) =
  let getId k =
    if Set.member k idsToPreserve
    then getId (k+1)
    else k
  in
  let assignIds pat k =
    if Set.member pat.val.pid idsToPreserve then
      (pat, k)
    else
      let pid = getId k in
      (setPId pid pat, pid + 1)
  in
  mapFoldPatTopDown assignIds initK p

maxId : Exp -> Int
maxId exp =
  let ids = allIds exp in
  List.maximum (initK :: Set.toList ids) |> U.fromJust_ "maxId"

-- Excludes EIds, PIds, and locIds less than initK (i.e. no prelude locs or dummy EIds)
allIds : Exp -> Set.Set Int
allIds exp = duplicateAndAllIds exp |> Tuple.second

-- Raw list of all ids
allIdsRaw : Exp -> List Int
allIdsRaw exp =
  let pidsInPat pat   = flattenPatTree pat |> List.map (.val >> .pid) in
  let pidsInPats pats = pats |> List.concatMap pidsInPat in
  let pidsInDecls (Declarations _ types anns exps) =
    pidsInPats <| (types |> elemsOf |> List.map (\(LetType _ _ _ p _ _ t) -> p)) ++
    (anns  |> List.map (\(LetAnnotation _ _ p _ _ t) -> p)) ++
    (exps  |> elemsOf |> List.map (\(LetExp _ _ p _ _ t) -> p))
  in
  let flattened = flattenExpTree exp in
  let eids = flattened |> List.map expEId in
  let otherIds =
    flattened
    |> List.concatMap
        (\exp ->
          case (unwrapExp exp) of
            EConst ws n (locId, frozen, ident) wd -> [locId]
            EFun ws1 pats body ws2                -> pidsInPats pats
            ECase ws1 scrutinee branches ws2      -> pidsInPats (branchPats branches)
            ELet ws1 kind declarations ws2 body   -> pidsInDecls declarations
            ERecord _ _ declarations _            -> pidsInDecls declarations
            _                                     -> []
        )
  in
  eids ++ otherIds

-- Excludes EIds, PIds, and locIds less than initK (i.e. no prelude locs or dummy EIds)
duplicateAndAllIds : Exp -> (Set.Set Int, Set.Set Int)
duplicateAndAllIds exp =
  allIdsRaw exp
  |> List.foldl
      (\id (duplicateIds, seenIds) ->
        if id >= initK then
          if Set.member id seenIds
          then (Set.insert id duplicateIds, seenIds)
          else (duplicateIds, Set.insert id seenIds)
        else
          (duplicateIds, seenIds)
      )
      (Set.empty, Set.empty)

preludeSubst = substPlusOf_ Dict.empty prelude

substPlusOf : Exp -> SubstPlus
substPlusOf e =
  substPlusOf_ preludeSubst e

substOf : Exp -> Subst
substOf = Dict.map (always .val) << substPlusOf

substStrOf : Exp -> SubstStr
substStrOf = Dict.map (always toString) << substOf


-- Record the primary identifier in the EConsts_ Locs, where appropriate.
recordIdentifiers : (Pat, Exp) -> Exp
recordIdentifiers (p, (Expr e)) =
 let exp = Expr e in
 let ret e__ = Expr <| WithInfo (Exp_ e__ <| expEId exp) e.start e.end in
 case (p.val.p__, unwrapExp exp) of

  -- (PVar _ x _, EConst ws n (k, b, "") wd) -> ret <| EConst ws n (k, b, x) wd
  (PVar _ x _, EConst ws n (k, b, _) wd) -> ret <| EConst ws n (k, b, x) wd

  (PList _ ps _ mp _, EList ws1 es ws2 me ws3) ->
    case U.maybeZip ps (U.listValues es) of
      Nothing  -> ret <| EList ws1 es ws2 me ws3
      Just pes -> let es_ = List.map recordIdentifiers pes in
                  let me_ =
                    case (mp, me) of
                      (Just p1, Just e1) -> Just (recordIdentifiers (p1,e1))
                      _                  -> me in
                  ret <| EList ws1 (U.listValuesMake es es_) ws2 me_ ws3

  (PAs _ p1 _ p_, _) -> recordIdentifiers (p1, recordIdentifiers (p_,exp))

  (_, EColonType ws1 e1 ws2 t ws3) ->
    ret <| EColonType ws1 (recordIdentifiers (p,e1)) ws2 t ws3

  (_, e__) -> ret e__

-- this will be done while parsing eventually...

substPlusOf_ : SubstPlus -> Exp -> SubstPlus
substPlusOf_ substPlus exp =
  let accumulator (Expr e) s =
    case unwrapExp <| Expr e of
      EConst _ n (locId,_,_) _ ->
        case Dict.get locId s of
          Nothing ->
            Dict.insert locId { e | val = n } s
          Just existing ->
            if n == existing.val then
              s
            else
              Debug.crash <| "substPlusOf_ Duplicate locId " ++ toString locId ++ " with differing value " ++ toString n ++ "\n" ++ LangUnparser.unparseWithIds exp
      _ -> s
  in
  foldExp accumulator substPlus exp

