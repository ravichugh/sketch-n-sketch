(* Make ('a, 'e) result yojson-compatible *)

type ('a, 'e) rresult = ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

type ('a, 'e) result = ('a, 'e) rresult =
  | Ok of 'a
  | Error of 'e
  [@@deriving yojson]

(* API *)

type eval_request =
  Lang.exp
  [@@deriving yojson]

type eval_response' =
  { res : Lang.res
  ; assertions: Lang.resumption_assertions
  }
  [@@deriving yojson]

type eval_response =
  (eval_response', string) result
  [@@deriving yojson]

type synthesis_request =
  { delta : Lang.hole_ctx
  ; sigma : Lang.datatype_ctx
  ; assertions : Lang.resumption_assertions
  }
  [@@deriving yojson]

type synthesis_response =
  { time_taken : float
  ; hole_fillings : (Lang.hole_name * Lang.exp) list list
  ; timed_out : bool
  }
  [@@deriving yojson]

(* Main *)

let synthesis_pipeline delta sigma assertions =
  assertions
    |> Uneval.simplify_assertions delta sigma
    |> Solve.solve_any delta sigma

let usage n =
  prerr_endline ("usage: " ^ Sys.argv.(0) ^ " eval|synthesize");
  exit n

let read_all file =
  let acc =
    ref []
  in
    begin try
      while true do
        acc := input_line file :: !acc
      done;
      !acc
    with
      End_of_file ->
        !acc
    end
      |> List.rev
      |> String.concat "\n"

let () =
  let () =
    if Array.length Sys.argv <> 2 then
      usage 1
    else
      ()
  in
  let command =
    Sys.argv.(1)
  in
  let user_input =
    read_all stdin
  in
  let handle decode encode callback =
    user_input
      |> Yojson.Safe.from_string
      |> decode
      |> Result2.map callback
      |> result_to_yojson encode (fun e -> `String e)
      |> Yojson.Safe.to_string
      |> print_endline
  in
  match command with
    | "eval" ->
        handle eval_request_of_yojson eval_response_to_yojson @@
          fun exp ->
            let () =
              Log.info "Evaluating..."
            in
            let () =
              Timer.Single.start Timer.Single.Total
            in
            let response =
              Result2.map (fun (res, assertions) -> {res; assertions}) @@
                Eval.eval [] exp
            in
            let time_taken =
              Timer.Single.elapsed Timer.Single.Total;
            in
            let () =
              Log.info
                ( "Completed in "
                    ^ string_of_float time_taken
                    ^ " seconds.\n"
                )
            in
              response

    | "synthesize" ->
        handle synthesis_request_of_yojson synthesis_response_to_yojson @@
          fun {delta; sigma; assertions} ->
            let () =
              Log.info "Synthesizing..."
            in
            let () =
              Term_gen.clear_cache ()
            in
            let clean_delta =
              List.map
                ( Pair2.map_snd @@ fun (gamma, tau, dec, match_depth) ->
                    ( List.filter
                        (fst >> Type.ignore_binding >> not)
                        gamma
                    , tau
                    , dec
                    , match_depth
                    )
                )
                delta
            in
            let () =
              clean_delta
                |> List.map fst
                |> List2.maximum
                |> Option2.with_default 0
                |> Fresh.set_largest_hole
            in
            let () =
              Uneval.minimal_uneval := true
            in
            let () =
              Timer.Single.start Timer.Single.Total;
            in
            let minimal_synthesis_result =
              synthesis_pipeline clean_delta sigma assertions
            in
            let synthesis_result =
              if Nondet.is_empty minimal_synthesis_result then
                let () =
                  Uneval.minimal_uneval := false
                in
                  synthesis_pipeline clean_delta sigma assertions
              else
                minimal_synthesis_result
            in
            let time_taken =
              Timer.Single.elapsed Timer.Single.Total;
            in
            let timed_out =
              time_taken > Params.max_total_time
            in
            let () =
              if not timed_out then
                Log.info
                  ( "Completed in "
                      ^ string_of_float time_taken
                      ^ " seconds.\n"
                  )
              else
                Log.info
                  ( "Timed out after "
                      ^ string_of_float time_taken
                      ^ " seconds.\n"
                  )
            in
              { time_taken
              ; hole_fillings =
                  synthesis_result
                    |> Nondet.map (fst >> Clean.clean clean_delta)
                    |> Nondet.collapse_option
                    |> Nondet.to_list
              ; timed_out
              }

    | _ ->
      usage 2
