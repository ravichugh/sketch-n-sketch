open Lwt
open Cohttp
open Cohttp_lwt_unix

let add (m : int) (n : int) =
	m + n

let server =
  let callback _conn req body =
    let
      _meth =
        req
          |> Request.meth
          |> Code.string_of_method
    in
    let
      _headers =
        req
          |> Request.headers
          |> Header.to_string
    in
      Cohttp_lwt.Body.to_string body >|=
      ( fun body ->
          Printf.sprintf "%s\n" body
      ) >>=
      ( fun body ->
          Server.respond_string ~status:`OK ~body ()
      )
  in
    Server.create
      ~mode:(`TCP (`Port 9090))
      (Server.make ~callback ())

let () =
  ignore (Lwt_main.run server)
