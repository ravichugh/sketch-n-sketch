module FastParser exposing
  ( prelude, isPreludeLoc, isPreludeLocId, isPreludeEId
  , substOf, substStrOf, substPlusOf
  , parseE, parseT
  , clearAllIds
  , freshen
  , maxId
  , showError
  )

import List
import Char

import String exposing (fromChar)
import Set exposing (Set)
import Dict exposing (Dict)

import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Parser.LowLevel exposing (getPosition, getOffset, getSource)

import Utils as U
import PreludeGenerated as Prelude
import Lang exposing (..)

--==============================================================================
--= PARSER WITH INFO
--==============================================================================

type alias ParserI a = Parser (WithInfo a)

--==============================================================================
--= HELPERS
--==============================================================================

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

singleton : a -> List a
singleton x = [x]

--------------------------------------------------------------------------------
-- Parser Combinators
--------------------------------------------------------------------------------

try : Parser a -> Parser a
try parser =
  delayedCommitMap always parser (succeed ())

token : String -> Parser String
token s =
  map (always s) (keyword s)

guard : String -> Bool -> Parser ()
guard failReason pred =
  if pred then (succeed ()) else (fail failReason)

--------------------------------------------------------------------------------
-- Info Helpers
--------------------------------------------------------------------------------

getPos : Parser Pos
getPos =
  map posFromRowCol getPosition

trackInfo : Parser a -> ParserI a
trackInfo p =
  delayedCommitMap
    ( \start (a, end) ->
        withInfo a start end
    )
    getPos
    ( succeed (,)
        |= p
        |= getPos
    )

untrackInfo : ParserI a -> Parser a
untrackInfo =
  map (.val)

--------------------------------------------------------------------------------
-- Whitespace
--------------------------------------------------------------------------------

isSpace : Char -> Bool
isSpace c =
  c == ' ' || c == '\n'

isOnlySpaces : String -> Bool
isOnlySpaces =
  String.all isSpace

spaces : Parser WS
spaces =
  trackInfo <| keep zeroOrMore isSpace

guardSpace : ParserI ()
guardSpace =
  trackInfo
    ( ( succeed (,)
        |= getOffset
        |= getSource
      )
      |> andThen
      ( \(offset, source) ->
          guard "expecting space" <|
            isOnlySpaces <| String.slice offset (offset + 1) source
      )
    )

spacedKeyword : String -> ParserI ()
spacedKeyword kword =
  trackInfo <|
    succeed ()
      |. keyword kword
      |. guardSpace

spaceSaverKeyword : String -> (WS -> a) -> ParserI a
spaceSaverKeyword kword combiner =
  delayedCommitMap
    ( \ws _ ->
        withInfo (combiner ws) ws.start ws.end
    )
    ( spaces )
    ( keyword kword )


spacesBefore : (WS -> a -> b) -> ParserI a -> ParserI b
spacesBefore combiner p =
  delayedCommitMap
    ( \ws x ->
        withInfo (combiner ws x.val) x.start x.end
    )
    spaces
    p

--------------------------------------------------------------------------------
-- Block Helper
--------------------------------------------------------------------------------

block
  : (WS -> a -> WS -> b) -> String -> String -> Parser a -> ParserI b
block combiner openSymbol closeSymbol p =
  delayedCommitMap
    ( \(wsStart, open) (result, wsEnd, close) ->
        withInfo
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
              |= trackInfo (token "-")
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
        List.map token [frozen, thawed, assignOnlyOnce, unann]

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
    , "pow"
    , "arctan2"
    , "if"
    , "case"
    , "typecase"
    , "let"
    , "letrec"
    , "def"
    , "defrec"
    , "typ"
    , "empty"
    , "insert"
    , "get"
    , "remove"
    , "debug"
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
-- General Pattern Helpers
--------------------------------------------------------------------------------

mapPat_ : ParserI Pat__ -> Parser Pat
mapPat_ = (map << mapInfo) pat_

--------------------------------------------------------------------------------
-- Name Pattern Helper
--------------------------------------------------------------------------------

namePattern : ParserI Ident -> Parser Pat
namePattern ident =
  mapPat_ <|
    spacesBefore (\ws name -> PVar ws name noWidgetDecl) ident

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
      spacesBefore PConst num

--------------------------------------------------------------------------------
-- Base Value Pattern
--------------------------------------------------------------------------------

baseValuePattern : Parser Pat
baseValuePattern =
  mapPat_ <|
    inContext "base value pattern" <|
      spacesBefore PBase baseValue

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
              withInfo (PAs wsStart name.val wsAt pat) name.start pat.end
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
          withInfo (combiner ws) ws.start ws.end
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
-- Named Types
--------------------------------------------------------------------------------

namedType : Parser Type
namedType =
  inContext "named type" <|
    spacesBefore TNamed typeIdentifierString

--------------------------------------------------------------------------------
-- Variable Types
--------------------------------------------------------------------------------

variableType : Parser Type
variableType =
  inContext "variable type" <|
    spacesBefore TVar variableIdentifierString

--------------------------------------------------------------------------------
-- Function Type
--------------------------------------------------------------------------------

functionType : Parser Type
functionType =
  lazy <| \_ ->
    inContext "function type" <|
      parenBlock TArrow <|
        succeed identity
          |. spacedKeyword "->"
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
          |. spacedKeyword "List"
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
            |. spacedKeyword "TDict"
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
    wsIdentifierPair =
      delayedCommitMap
        ( \ws name ->
            (ws, name.val)
        )
        spaces
        variableIdentifierString
    quantifiers =
      oneOf
        [ inContext "forall type (one)" <|
            map One wsIdentifierPair
        , inContext "forall type (many) "<|
            untrackInfo <|
              parenBlock Many <|
                repeat zeroOrMore wsIdentifierPair
        ]
  in
    inContext "forall type" <|
      lazy <| \_ ->
        parenBlock
          ( \wsStart (qs, t) wsEnd ->
              TForall wsStart qs t wsEnd
          )
          ( succeed (,)
              |. spacedKeyword "forall"
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
          |. spacedKeyword "union"
          |= repeat oneOrMore typ

--------------------------------------------------------------------------------
-- Wildcard Type
--------------------------------------------------------------------------------

wildcardType : Parser Type
wildcardType =
  inContext "wildcard type" <|
    spaceSaverKeyword "_" TWildcard

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
-- General Expression Helpers
--------------------------------------------------------------------------------

mapExp_ : ParserI Exp__ -> Parser Exp
mapExp_ = (map << mapInfo) exp_

--------------------------------------------------------------------------------
-- Identifier Expressions
--------------------------------------------------------------------------------

variableExpression : Parser Exp
variableExpression =
  mapExp_ <|
    spacesBefore EVar variableIdentifierString

--------------------------------------------------------------------------------
-- Constant Expressions
--------------------------------------------------------------------------------

-- TODO interacts badly with auto-abstracted variable names...
dummyLocWithDebugInfo : Frozen -> Num -> Loc
dummyLocWithDebugInfo b n = (0, b, "")

constantExpression : Parser Exp
constantExpression =
  mapExp_ <|
    delayedCommitMap
      ( \ws (n, fa, w) ->
          withInfo
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

baseValueExpression : Parser Exp
baseValueExpression =
  mapExp_ <|
    spacesBefore EBase baseValue

--------------------------------------------------------------------------------
-- Primitive Operators
--------------------------------------------------------------------------------

operator : Parser Exp
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
                |. keyword "empty"
              -- Non-unary operators
            , succeed Cos
                |. spacedKeyword "cos"
            , succeed Sin
                |. spacedKeyword "sin"
            , succeed ArcCos
                |. spacedKeyword "arccos"
            , succeed ArcSin
                |. spacedKeyword "arcsin"
            , succeed Floor
                |. spacedKeyword "floor"
            , succeed Ceil
                |. spacedKeyword "ceiling"
            , succeed Round
                |. spacedKeyword "round"
            , succeed ToStr
                |. spacedKeyword "toString"
            , succeed Sqrt
                |. spacedKeyword "sqrt"
            , succeed Explode
                |. spacedKeyword "explode"
            , succeed Plus
                |. spacedKeyword "+"
            , succeed Minus
                |. spacedKeyword "-"
            , succeed Mult
                |. spacedKeyword "*"
            , succeed Div
                |. spacedKeyword "/"
            , succeed Lt
                |. spacedKeyword "<"
            , succeed Eq
                |. spacedKeyword "="
            , succeed Mod
                |. spacedKeyword "mod"
            , succeed Pow
                |. spacedKeyword "pow"
            , succeed ArcTan2
                |. spacedKeyword "arctan2"
            , succeed DictInsert
                |. spacedKeyword "insert"
            , succeed DictGet
                |. spacedKeyword "get"
            , succeed DictRemove
                |. spacedKeyword "remove"
            , succeed DebugLog
                |. spacedKeyword "debug"
            ]
    in
      inContext "operator" <|
        lazy <| \_ ->
          parenBlock
            ( \wsStart (opName, args) wsEnd ->
                EOp wsStart opName args wsEnd
            )
            ( succeed (,)
                |= op
                |= repeat zeroOrMore exp
            )

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : Parser Exp
conditional =
  mapExp_ <|
    inContext "conditional" <|
      lazy <| \_ ->
        parenBlock
          ( \wsStart (c, a, b) wsEnd ->
              EIf wsStart c a b wsEnd
          )
          ( succeed (,,)
             |. spacedKeyword "if"
             |= exp
             |= exp
             |= exp
          )

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

list : Parser Exp
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
                EList wsStart heads space0 Nothing wsEnd
            )
        , multiConsCombiner =
            ( \wsStart heads wsBar tail wsEnd ->
                EList wsStart heads wsBar (Just tail) wsEnd
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
  -> Parser Exp
genericCase context kword combiner branchCombiner parser branchParser =
  let
    path =
      inContext (context ++ " path") <|
        lazy <| \_ ->
          parenBlock
            ( \wsStart (p, e) wsEnd ->
                branchCombiner wsStart p e wsEnd
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
                |. spacedKeyword kword
                |= parser
                |= repeat zeroOrMore path
            )

--------------------------------------------------------------------------------
-- Case Expressions
--------------------------------------------------------------------------------

caseExpression : Parser Exp
caseExpression =
    lazy <| \_ ->
      genericCase
        "case expression" "case"
        ECase Branch_ exp pattern

--------------------------------------------------------------------------------
-- Type Case Expressions
--------------------------------------------------------------------------------

typeCaseExpression : Parser Exp
typeCaseExpression =
    lazy <| \_ ->
      genericCase
        "type case expression" "typecase"
        ETypeCase TBranch_ pattern typ

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

function : Parser Exp
function =
  let
    parameters =
      oneOf
        [ map singleton pattern
        , untrackInfo <| parenBlockIgnoreWS <| repeat oneOrMore pattern
        ]
  in
    mapExp_ <|
      inContext "function" <|
        lazy <| \_ ->
          parenBlock
            ( \wsStart (params, body) wsEnd ->
                EFun wsStart params body wsEnd
            )
            ( succeed (,)
                |. symbol "\\"
                |= parameters
                |= exp
            )

--------------------------------------------------------------------------------
-- Function Applications
--------------------------------------------------------------------------------

functionApplication : Parser Exp
functionApplication =
  mapExp_ <|
    inContext "function application" <|
      lazy <| \_ ->
        parenBlock
          ( \wsStart (f, x) wsEnd ->
              EApp wsStart f x wsEnd
          )
          ( succeed (,)
              |= exp
              |= repeat oneOrMore exp
          )

--------------------------------------------------------------------------------
-- Let Bindings
--------------------------------------------------------------------------------

genericLetBinding : String -> String -> Bool -> Parser Exp
genericLetBinding context kword isRec =
  mapExp_ <|
    inContext context <|
      parenBlock
        ( \wsStart (name, binding, rest) wsEnd ->
            ELet wsStart Let isRec name binding rest wsEnd
        )
        ( succeed (,,)
            |. spacedKeyword kword
            |= pattern
            |= exp
            |= exp
        )

genericDefBinding : String -> String -> Bool -> Parser Exp
genericDefBinding context kword isRec =
  mapExp_ <|
    inContext context <|
      delayedCommitMap
        ( \(wsStart, open) (name, binding, wsEnd, close, rest) ->
            withInfo
              (ELet wsStart Def isRec name binding rest wsEnd)
              open.start
              close.end
        )
        ( succeed (,)
            |= spaces
            |= trackInfo (symbol "(")
        )
        ( succeed (,,,,)
            |. spacedKeyword kword
            |= pattern
            |= exp
            |= spaces
            |= trackInfo (symbol ")")
            |= exp
        )

recursiveLetBinding : Parser Exp
recursiveLetBinding =
  lazy <| \_ ->
    genericLetBinding "recursive let binding" "letrec" True

simpleLetBinding : Parser Exp
simpleLetBinding =
  lazy <| \_ ->
    genericLetBinding "non-recursive let binding" "let" False

recursiveDefBinding : Parser Exp
recursiveDefBinding =
  lazy <| \_ ->
    genericDefBinding "recursive def binding" "defrec" True

simpleDefBinding : Parser Exp
simpleDefBinding =
  lazy <| \_ ->
    genericDefBinding "non-recursive def binding" "def" False

letBinding : Parser Exp
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
-- Options
--------------------------------------------------------------------------------

option : Parser Exp
option =
  mapExp_ <|
    inContext "option" <|
      lazy <| \_ ->
        delayedCommitMap
          ( \wsStart (open, opt, wsMid, val, rest) ->
              withInfo
                (EOption wsStart opt wsMid val rest)
                open.start
                val.end
          )
          ( spaces )
          ( succeed (,,,,)
              |= trackInfo (symbol "#")
              |. spaces
              |= trackInfo
                   ( keep zeroOrMore <| \c ->
                       c /= '\n' && c /= ' ' && c /= ':'
                   )
              |. symbol ":"
              |= spaces
              |= trackInfo
                   ( keep zeroOrMore <| \c ->
                       c /= '\n'
                   )
              |. symbol "\n"
              |= exp
          )

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

typeDeclaration : Parser Exp
typeDeclaration =
  mapExp_ <|
    inContext "type declaration" <|
      delayedCommitMap
        ( \(wsStart, open) (pat, t, wsEnd, close, rest) ->
            withInfo
              (ETyp wsStart pat t rest wsEnd)
              open.start
              close.end
        )
        ( succeed (,)
            |= spaces
            |= trackInfo (symbol "(")
        )
        ( succeed (,,,,)
            |. spacedKeyword "typ"
            |= variablePattern
            |= typ
            |= spaces
            |= trackInfo (symbol ")")
            |= exp
        )

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

typeAlias : Parser Exp
typeAlias =
  mapExp_ <|
    inContext "type alias" <|
      delayedCommitMap
        ( \(wsStart, open, pat) (t, wsEnd, close, rest) ->
            withInfo
              (ETypeAlias wsStart pat t rest wsEnd)
              open.start
              close.end
        )
        ( succeed (,,)
            |= spaces
            |= trackInfo (symbol "(")
            |. spacedKeyword "def "
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

typeAnnotation : Parser Exp
typeAnnotation =
  mapExp_ <|
    inContext "type annotation" <|
      lazy <| \_ ->
        parenBlock
          ( \wsStart (e, wsColon, t) wsEnd ->
              EColonType wsStart e wsColon t wsEnd
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
-- Comments
--------------------------------------------------------------------------------

comment : Parser Exp
comment =
  mapExp_ <|
    inContext "comment" <|
      lazy <| \_ ->
        delayedCommitMap
          ( \wsStart (semicolon, text, rest) ->
              withInfo
                (EComment wsStart text.val rest)
                semicolon.start
                text.end
          )
          spaces
          ( succeed (,,)
              |= trackInfo (symbol ";")
              |= trackInfo (keep zeroOrMore (\c -> c /= '\n'))
              |. symbol "\n"
              |= exp
          )

--------------------------------------------------------------------------------
-- General Expressions
--------------------------------------------------------------------------------

exp : Parser Exp
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
        , lazy <| \_ -> comment
        , lazy <| \_ -> option
        , variableExpression
        ]

--==============================================================================
--= TOP-LEVEL EXPRESSIONS
--==============================================================================

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

type alias TopLevelExp = WithInfo (Exp -> Exp_)

--------------------------------------------------------------------------------
-- Top-Level Expression Fusing
--------------------------------------------------------------------------------

fuseTopLevelExp : TopLevelExp -> Exp -> Exp
fuseTopLevelExp tld rest =
  withInfo (tld.val rest) tld.start tld.end

fuseTopLevelExps : (List TopLevelExp) -> Exp -> Exp
fuseTopLevelExps tlds rest =
  List.foldr fuseTopLevelExp rest tlds

--------------------------------------------------------------------------------
-- Top-Level Defs
--------------------------------------------------------------------------------

genericTopLevelDef : String -> String -> Bool -> Parser TopLevelExp
genericTopLevelDef context kword isRec =
  inContext context <|
    parenBlock
      ( \wsStart (name, binding) wsEnd ->
          ( \rest ->
              exp_ <| ELet wsStart Def isRec name binding rest wsEnd
          )
      )
      ( succeed (,)
          |. spacedKeyword kword
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
              exp_ <| ETyp wsStart pat t rest wsEnd
          )
      )
      ( succeed (,)
          |. spacedKeyword "typ"
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
          withInfo
            (\rest -> (exp_ <| ETypeAlias wsStart pat t rest wsEnd))
            open.start
            close.end
      )
      ( succeed (,,)
          |= spaces
          |= trackInfo (symbol "(")
          |. spacedKeyword "def"
          |= typePattern
      )
      ( succeed (,,)
          |= typ
          |= spaces
          |= trackInfo (symbol ")")
      )

--------------------------------------------------------------------------------
-- Top-Level Comments
--------------------------------------------------------------------------------

topLevelComment : Parser TopLevelExp
topLevelComment =
  inContext "top-level comment" <|
    delayedCommitMap
      ( \wsStart (semicolon, text) ->
          withInfo
            ( \rest ->
                exp_ <| EComment wsStart text.val rest
            )
            semicolon.start
            text.end
      )
      spaces
      ( succeed (,)
          |= trackInfo (symbol ";")
          |= trackInfo (keep zeroOrMore (\c -> c /= '\n'))
          |. symbol "\n"
      )

--------------------------------------------------------------------------------
-- Top-Level Options
--------------------------------------------------------------------------------

topLevelOption : Parser TopLevelExp
topLevelOption =
  inContext "top-level option" <|
    spacesBefore
      ( \wsStart (opt, wsMid, val) ->
          ( \rest ->
              exp_ <| EOption wsStart opt wsMid val rest
          )
      )
      ( trackInfo <| succeed (,,)
          |. symbol "#"
          |. spaces
          |= trackInfo
               ( keep zeroOrMore <| \c ->
                   c /= '\n' && c /= ' ' && c /= ':'
               )
          |. symbol ":"
          |= spaces
          |= trackInfo
               ( keep zeroOrMore <| \c ->
                   c /= '\n'
               )
          |. symbol "\n"
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
      , topLevelComment
      , topLevelOption
      ]

allTopLevelExps : Parser (List TopLevelExp)
allTopLevelExps =
  repeat zeroOrMore topLevelExp

--==============================================================================
--= PROGRAMS
--==============================================================================

-- TODO there's a deuce polygon drawn from somewhere in this expression
-- to dummyPos. fix this.
implicitMain =
  eLet [("_IMPLICIT_MAIN", eStr "...")] (eVar "main")
    |> succeed
    |> untrackInfo
    |> trackInfo

mainExp =
  oneOf [exp, implicitMain]
    -- if using implicitMain, the last topLevelExp (e.g. topLevelComment)
    -- requires a newline. TODO fix this.

program : Parser Exp
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
parseE_ f = run (map f program)

parseE : String -> Result Error Exp
parseE = parseE_ freshen

parseT : String -> Result Error Type
parseT = run typ

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

showIndentedProblem : Int -> Problem -> String
showIndentedProblem n prob =
  let
    indent =
      String.repeat (2 * n) " "
  in
    case prob of
      BadOneOf probs ->
        indent ++ "One of:\n" ++
          String.concat (List.map (showIndentedProblem (n + 1)) probs)
      BadInt ->
        indent ++ "Bad integer value\n"
      BadFloat ->
        indent ++ "Bad float value\n"
      BadRepeat ->
        indent ++ "Parse of zero-length input indefinitely\n"
      ExpectingEnd ->
        indent ++ "Expecting end\n"
      ExpectingSymbol s ->
        indent ++ "Expecting symbol '" ++ s ++ "'\n"
      ExpectingKeyword s ->
        indent ++ "Expecting keyword '" ++ s ++ "'\n"
      ExpectingVariable ->
        indent ++ "Expecting variable\n"
      ExpectingClosing s ->
        indent ++ "Expecting closing string '" ++ s ++ "'\n"
      Fail s ->
        indent ++ "Parser failure: " ++ s ++ "\n"

showError : Error -> String
showError err =
  let
    prettyError =
      let
        sourceLines =
          String.lines err.source
        problemLine =
          List.head (List.drop (err.row - 1) sourceLines)
        arrow =
          (String.repeat (err.col - 1) " ") ++ "^"
      in
        case problemLine of
          Just line ->
            line ++ "\n" ++ arrow ++ "\n\n"
          Nothing ->
            ""
    showContext c =
      "  (row: " ++ (toString c.row) ++", col: " ++ (toString c.col)
      ++ ") Error while parsing '" ++ c.description ++ "'\n"
    deepestContext =
      case List.head err.context of
        Just c ->
          "Error while parsing '" ++ c.description ++ "':\n"
        Nothing ->
          ""
  in
    "[Parser Error]\n\n" ++
      deepestContext ++ "\n" ++
      prettyError ++
    "Position\n" ++
    "========\n" ++
    "  Row: " ++ (toString err.row) ++ "\n" ++
    "  Col: " ++ (toString err.col) ++ "\n\n" ++
    "Problem\n" ++
    "=======\n" ++
      (showIndentedProblem 1 err.problem) ++ "\n" ++
    "Context Stack\n" ++
    "=============\n" ++
      (String.concat <| List.map showContext err.context) ++ "\n\n"

--------------------------------------------------------------------------------
-- Code from old parser
--------------------------------------------------------------------------------

(prelude, initK) =
  freshenClean 1 <| U.fromOkay "parse prelude" <| parseE_ identity Prelude.src

preludeIds = allIds prelude

isPreludeLoc : Loc -> Bool
isPreludeLoc (k,_,_) = isPreludeLocId k

isPreludeLocId : LocId -> Bool
isPreludeLocId k = k < initK

isPreludeEId : EId -> Bool
isPreludeEId k = k < initK

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
  let startK      = (List.maximum (initK :: Set.toList allIds) |> U.fromJust) + 1 in
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
  let assignIds exp k =
    let e__ = exp.val.e__ in
    let (newE__, newK) =
      case e__ of
        EConst ws n (locId, frozen, ident) wd ->
          if Set.member locId idsToPreserve then
            (e__, k)
          else
            let locId = getId k in
            (EConst ws n (locId, frozen, ident) wd, locId + 1)

        ELet ws1 kind b p e1 e2 ws2 ->
          let (newP, newK) = freshenPatPreserving idsToPreserve k p in
          let newE1 = recordIdentifiers (newP, e1) in
          (ELet ws1 kind b newP newE1 e2 ws2, newK)

        EFun ws1 pats body ws2 ->
          let (newPats, newK) = freshenPatsPreserving idsToPreserve k pats in
          (EFun ws1 newPats body ws2, newK)

        ECase ws1 scrutinee branches ws2 ->
          let (newBranches, newK) =
            branches
            |> List.foldl
                (\branch (newBranches, k) ->
                  let (Branch_ bws1 pat ei bws2) = branch.val in
                  let (newPi, newK) = freshenPatPreserving idsToPreserve k pat in
                  (newBranches ++ [{ branch | val = Branch_ bws1 newPi ei bws2 }], newK)
                )
                ([], k)
          in
          (ECase ws1 scrutinee newBranches ws2, newK)

        -- TypeCase scrutinee will be changed to an exp eventually.
        ETypeCase ws1 pat tbranches ws2 ->
          let (newPat, newK) = freshenPatPreserving idsToPreserve k pat in
          (ETypeCase ws1 newPat tbranches ws2, newK)

        ETyp ws1 pat tipe e ws2 ->
          let (newPat, newK) = freshenPatPreserving idsToPreserve k pat in
          (ETyp ws1 newPat tipe e ws2, newK)

        ETypeAlias ws1 pat tipe e ws2 ->
          let (newPat, newK) = freshenPatPreserving idsToPreserve k pat in
          (ETypeAlias ws1 newPat tipe e ws2, newK)

        _ ->
          (e__, k)
    in
    if Set.member exp.val.eid idsToPreserve then
      (replaceE__ exp newE__, newK)
    else
      let eid = getId newK in
      (WithInfo (Exp_ newE__ eid) exp.start exp.end, eid + 1)
  in
  mapFoldExp assignIds initK e


-- Reassign any id not in idsToPreserve
freshenPatsPreserving : Set.Set Int -> Int -> List Pat -> (List Pat, Int)
freshenPatsPreserving idsToPreserve initK pats =
  pats
  |> List.foldl
      (\pat (finalPats, k) ->
        let (newPat, newK) = freshenPatPreserving idsToPreserve k pat in
        (finalPats ++ [newPat], newK)
      )
      ([], initK)


-- Reassign any id not in idsToPreserve
freshenPatPreserving : Set.Set Int -> Int -> Pat -> (Pat, Int)
freshenPatPreserving idsToPreserve initK p =
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
  List.maximum (initK :: Set.toList ids) |> U.fromJust

-- Excludes EIds, PIds, and locIds less than initK (i.e. no prelude locs or dummy EIds)
allIds : Exp -> Set.Set Int
allIds exp = duplicateAndAllIds exp |> Tuple.second

-- Raw list of all ids
allIdsRaw : Exp -> List Int
allIdsRaw exp =
  let pidsInPat pat   = flattenPatTree pat |> List.map (.val >> .pid) in
  let pidsInPats pats = pats |> List.concatMap pidsInPat in
  let flattened = flattenExpTree exp in
  let eids = flattened |> List.map (.val >> .eid) in
  let otherIds =
    flattened
    |> List.concatMap
        (\exp ->
          case exp.val.e__ of
            EConst ws n (locId, frozen, ident) wd -> [locId]
            ELet ws1 kind b p e1 e2 ws2           -> pidsInPat p
            EFun ws1 pats body ws2                -> pidsInPats pats
            ECase ws1 scrutinee branches ws2      -> pidsInPats (branchPats branches)
            ETypeCase ws1 pat tbranches ws2       -> pidsInPat pat
            ETyp ws1 pat tipe e ws2               -> pidsInPat pat
            ETypeAlias ws1 pat tipe e ws2         -> pidsInPat pat
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
recordIdentifiers (p,e) =
 let ret e__ = WithInfo (Exp_ e__ e.val.eid) e.start e.end in
 case (p.val.p__, e.val.e__) of

  -- (PVar _ x _, EConst ws n (k, b, "") wd) -> ret <| EConst ws n (k, b, x) wd
  (PVar _ x _, EConst ws n (k, b, _) wd) -> ret <| EConst ws n (k, b, x) wd

  (PList _ ps _ mp _, EList ws1 es ws2 me ws3) ->
    case U.maybeZip ps es of
      Nothing  -> ret <| EList ws1 es ws2 me ws3
      Just pes -> let es_ = List.map recordIdentifiers pes in
                  let me_ =
                    case (mp, me) of
                      (Just p1, Just e1) -> Just (recordIdentifiers (p1,e1))
                      _                  -> me in
                  ret <| EList ws1 es_ ws2 me_ ws3

  (PAs _ _ _ p_, _) -> recordIdentifiers (p_,e)

  (_, EColonType ws1 e1 ws2 t ws3) ->
    ret <| EColonType ws1 (recordIdentifiers (p,e1)) ws2 t ws3

  (_, e__) -> ret e__

-- this will be done while parsing eventually...

substPlusOf_ : SubstPlus -> Exp -> SubstPlus
substPlusOf_ substPlus exp =
  let accumulator e s =
    case e.val.e__ of
      EConst _ n (locId,_,_) _ ->
        case Dict.get locId s of
          Nothing ->
            Dict.insert locId { e | val = n } s
          Just existing ->
            if n == existing.val then s else Debug.crash <| "substPlusOf_ Constant: " ++ (toString n)
      _ -> s
  in
  foldExp accumulator substPlus exp

