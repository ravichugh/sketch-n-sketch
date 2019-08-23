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

let server =
  let callback _ _ body =
    let open Lwt in
    body
      |>  Cohttp_lwt.Body.to_string
      >|= Yojson.Safe.from_string
      >|= Lang.exp_of_yojson
      >>= begin function
            | Ok exp ->
                begin match Eval.eval [] exp with
                  | Ok (res, _) ->
                      respond_ok @@
                        "{\"Ok\": "
                          ^ Yojson.Safe.to_string (Lang.res_to_yojson res)
                          ^ "}"

                  | Error e ->
                      respond_ok @@
                        "{\"Error\": \"" ^ e ^ "\"}"
                end

            | Error e ->
                respond_ok @@
                  "{\"Error\": \"" ^ e ^ "\"}"
          end
  in
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port 9090))
      (Cohttp_lwt_unix.Server.make ~callback ())

let () =
  identity @@ ignore (Lwt_main.run server)
