module Types2 exposing
  ( makeDeuceTool
  , AceTypeInfo
  , aceTypeInfo
  , dummyAceTypeInfo
  )

import Info exposing (withDummyInfo)
import Lang exposing (..)
import LangTools
import ElmUnparser exposing (unparseType)
import Ace
-- can't depend on Model, since ExamplesGenerated depends on Types2

--------------------------------------------------------------------------------

type alias AceTypeInfo =
  { annotations : List Ace.Annotation
  , highlights : List Ace.Highlight
  , tooltips : List Ace.Tooltip
  }

dummyAceTypeInfo =
  AceTypeInfo [] [] []

aceTypeInfo : Exp -> AceTypeInfo
aceTypeInfo exp =
  { annotations = [ {row = 0, type_ = "warning", text="Type checking is on its way!"} ]
  , highlights = []
  , tooltips =
      let addTooltip e =
        { row = e.start.line - 1
        , col = e.start.col - 1
        , text = "EId: " ++ toString e.val.eid
        } 
      in
      foldExp (\e acc -> addTooltip e :: acc) [] exp
  }


--------------------------------------------------------------------------------

makeDeuceTool : Exp -> EId -> DeuceTransformation
makeDeuceTool inputExp eId = \() ->
  let
    show s =
      synthesisResult s inputExp

    tool =
      synthesisResult

    exp =
      LangTools.justFindExpByEId inputExp eId

    -- posInfo =
    --   [ show <| "Start: " ++ toString exp.start ++ " End: " ++ toString exp.end
    --   ]

    maybeTyp =
      case exp.val.e__ of
        EConst _ _ _ _ -> Just (withDummyInfo (TNum space1))
        EBase _ (EBool _) -> Just (withDummyInfo (TBool space1))
        EBase _ (EString _ _) -> Just (withDummyInfo (TString space1))
        EColonType _ _ _ t _ -> Just t
        _ -> Nothing

    typeInfo =
      case maybeTyp of
        Just typ -> [ show <| "Type: " ++ unparseType typ ]
        Nothing  -> []

    insertAnnotationTool =
      case maybeTyp of
        Just typ ->
          let e__ =
            EParens space1
                    (withDummyExpInfo (EColonType space0 exp space1 typ space0))
                    Parens
                    space0
          in
          [ tool "Insert Annotation" (replaceExpNodeE__ByEId eId e__ inputExp) ]

        Nothing ->
          []
  in
    List.concat <|
      [ typeInfo
      , insertAnnotationTool
      ]
