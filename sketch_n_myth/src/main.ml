open Lwt
open Cohttp_lwt_unix

let server =
  let callback _ _ body =
    body
      |>  Cohttp_lwt.Body.to_string
      >|= Yojson.Safe.from_string
      >|= Lang.exp_of_yojson
      >>= begin fun res ->
            match res with
              | Ok _ ->
                  Server.respond_string
                    ~status:`OK
                    ~body:"ok!\n"
                    ()
              | Error _ ->
                  Server.respond_string
                    ~status:`OK
                    ~body:"error!\n"
                    ()
          end
  in
    Server.create
      ~mode:(`TCP (`Port 9090))
      (Server.make ~callback ())

let () =
  ignore (Lwt_main.run server)
