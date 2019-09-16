(* Generic server code *)

let cors_headers =
  Cohttp.Header.add_list (Cohttp.Header.init ())
    [ ("Access-Control-Allow-Origin", "*")
    ; ("Access-Control-Allow-Headers", "Accept, Content-Type")
    ; ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    ]

let respond_ok body =
  Cohttp_lwt_unix.Server.respond_string
    ~status:`OK
    ~headers:cors_headers
    ~body
    ()

(* Make ('a, 'e) result yojson-compatible *)

type ('a, 'e) rresult = ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

type ('a, 'e) result = ('a, 'e) rresult =
  | Ok of 'a
  | Error of 'e
  [@@deriving yojson]

(* REST API *)

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

(* Server code *)

let server =
  let callback _ request body =
    Lwt.bind
      ( body
          |> Cohttp_lwt.Body.to_string
          |> Lwt.map Yojson.Safe.from_string
      ) @@ fun body_json ->
    let handle decode encode callback =
      body_json
        |> decode
        |> Result2.map callback
        |> result_to_yojson encode (fun e -> `String e)
        |> Yojson.Safe.to_string
        |> respond_ok
    in
    match Cohttp.Request.resource request with
      | "/eval" ->
          handle eval_request_of_yojson eval_response_to_yojson @@
            fun exp ->
              Log.info "Evaluating...";
              let initial_time =
                Timer.now ()
              in
              let response =
                Result2.map (fun (res, assertions) -> {res; assertions}) @@
                  Eval.eval [] exp
              in
              let final_time =
                Timer.now ()
              in
              let time_taken =
                final_time -. initial_time
              in
                Log.info
                  ( "Completed in "
                      ^ string_of_float time_taken
                      ^ " seconds.\n"
                  );
                response

      | "/synthesize" ->
          handle synthesis_request_of_yojson synthesis_response_to_yojson @@
            fun {delta; sigma; assertions} ->
              Log.info "Synthesizing...";
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
              let ((synthesis_result, timed_out), time_taken) =
                Timer.handle Timer.Total
                  begin fun () ->
                    assertions
                      |> Uneval.simplify clean_delta sigma
                      |> Solve.solve_any clean_delta sigma
                      |> (fun r -> (r, false))
                  end begin fun () ->
                    (Nondet.none, true)
                  end
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
          Cohttp_lwt_unix.Server.respond_not_found ()
  in
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port 9090))
      (Cohttp_lwt_unix.Server.make ~callback ())

let () =
  ignore (Lwt_main.run server)
