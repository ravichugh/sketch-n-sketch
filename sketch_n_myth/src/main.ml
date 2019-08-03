open Lwt
open Cohttp_lwt_unix

let server =
  let callback _ _ body =
    body
      |>  Cohttp_lwt.Body.to_string
      >|= Yojson.Safe.from_string
      >|= Lang.exp_of_yojson
      >>= begin function
            | Ok exp ->
                begin match Eval.eval [] exp with
                  | Ok (res, _) ->
                      let
                        output =
                          res
                            |> Lang.res_to_yojson
                            |> Yojson.Safe.to_string
                      in
                        Server.respond_string
                          ~status:`OK
                          ~body:output
                          ()

                  | Error e ->
                      Server.respond_string
                        ~status:`OK
                        ~body:("error: " ^ e)
                        ()
                end

            | Error _ ->
                Server.respond_string
                  ~status:`OK
                  ~body:("json type error")
                  ()
          end
  in
    Server.create
      ~mode:(`TCP (`Port 9090))
      (Server.make ~callback ())

let () =
  ignore (Lwt_main.run server)
