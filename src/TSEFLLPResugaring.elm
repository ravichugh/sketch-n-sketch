-- Convert from our core language for tiny structured editors back to Sketch-n-Sketch values.
module TSEFLLPResugaring exposing (..)

-- import Set exposing (Set)

-- import Javascript
import Lang
-- import LangTools
-- import Types2
import Update
import Utils
import ValBuilder

import TSEFLLPTypes exposing (..)


taggedValToLangValResult : TaggedValue -> Result String Lang.Val
taggedValToLangValResult taggedValue =
  let recurseAllResult = List.map taggedValToLangValResult >> Utils.projOk in
  case taggedValue.v of
    VClosure _ fName _ _   -> Err <| "Can't resugar closures (" ++ fName ++ ")."
    VClosureDynamic ident  -> Err <| "Can't resugar dynamic closures (" ++ ident ++ ")."
    VCtor "True" []        -> Ok <| ValBuilder.bool Lang.valNoProvenance True
    VCtor "False" []       -> Ok <| ValBuilder.bool Lang.valNoProvenance False
    VCtor "Nil" []         -> Ok <| Update.vList []
    VCtor "Cons" [x, tail] ->
      case recurseAllResult [x, tail] of
        Ok [xResugared, tailResugared] ->
          let tailVals = tailResugared |> Lang.vListToVals "TSEFLLPResugaring.taggedValToLangValResult" in
          Ok <| Update.vList (xResugared :: tailVals)

        Ok _  -> Err "taggedValToLangValResult: this should not happen!"
        Err s -> Err s

    VCtor ctorName argVals ->
      recurseAllResult argVals
      |> Result.map (\argLangVals -> ValBuilder.constructor Lang.valNoProvenance ctorName argLangVals)

    VString string -> Ok <| Update.vStr string
    VAppend w1 w2  -> Err "Can't desugar appends."
    VNum num       -> Ok <| Update.vConst (num, Lang.dummyTrace)
