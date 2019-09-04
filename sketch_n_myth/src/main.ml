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
              Result2.map (fun (res, assertions) -> {res; assertions}) @@
                Eval.eval [] exp

      | "/synthesize" ->
          handle synthesis_request_of_yojson synthesis_response_to_yojson @@
            fun {delta; sigma; assertions} ->
              let () =
                Term_gen.clear_cache ()
              in
              let initial_time =
                Sys.time ()
              in
              let params =
                { Lang.max_scrutinee_size = 3
                ; Lang.max_term_size = 10
                ; Lang.max_match_depth = 1
                }
              in
              let synthesis_result =
                assertions
                  |> Uneval.simplify delta sigma
                  |> Nondet.and_then (Solve.solve params delta sigma)
              in
              let final_time =
                Sys.time ()
              in
                { time_taken =
                    final_time -. initial_time
                ; hole_fillings =
                    synthesis_result
                      |> Nondet.map (fst >> Clean.clean delta)
                      |> Nondet.collapse_option
                      |> Nondet.to_list
                }

      | _ ->
          Cohttp_lwt_unix.Server.respond_not_found ()
  in
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port 9090))
      (Cohttp_lwt_unix.Server.make ~callback ())

let () =
  ignore (Lwt_main.run server)
