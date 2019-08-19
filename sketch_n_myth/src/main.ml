let respond_ok body =
  Cohttp_lwt_unix.Server.respond_string
    ~status:`OK
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
                      res
                        |> Lang.res_to_yojson
                        |> Yojson.Safe.to_string
                        |> respond_ok

                  | Error e ->
                      respond_ok @@ "error: " ^ e
                end

            | Error _ ->
                respond_ok "json type error"
          end
  in
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port 9090))
      (Cohttp_lwt_unix.Server.make ~callback ())

let () =
  identity @@ ignore (Lwt_main.run server)
