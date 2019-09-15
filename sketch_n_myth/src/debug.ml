open Lang

let debug func =
  if Params.debug_mode then
    func ()
  else
    ()

let println s = debug @@ fun _ ->
  print_endline s

let readln () = debug @@ fun _ ->
  let _ = read_line () in ()

let print_json j = debug @@ fun _ ->
  print_endline (Yojson.Safe.to_string j)

let print_hf hf = debug @@ fun _ ->
  hf
    |> Hole_map.bindings
    |> List.map
         ( fun (h, e) ->
             "??"
               ^ (string_of_int h)
               ^ ": "
               ^ (Yojson.Safe.to_string @@ exp_to_yojson e)
         )
    |> String.concat "\n"
    |> print_endline

let print_worlds worlds = debug @@ fun _ ->
  let s =
   String.concat "\n, " @@
    List.map
      ( fun (_env, ex) ->
          (* Yojson.Safe.to_string (env_to_yojson env)
            ^ "\n    ~ "
            ^ Yojson.Safe.to_string (example_to_yojson ex) *)
          Yojson.Safe.to_string (example_to_yojson ex)
      )
      worlds
  in
    print_endline @@ "{ " ^ s ^ "\n}"

let print_unsolved_constraints us = debug @@ fun _ ->
  us
    |> Hole_map.bindings
    |> List.map
         ( fun (h, ws) ->
             print_endline @@
               "??"
                 ^ (string_of_int h)
                 ^ ": ";
              print_worlds ws
         )
    |> (fun _ -> ())

let print_exp exp = debug @@ fun _ ->
  print_endline @@ Yojson.Safe.to_string (exp_to_yojson exp)

let print_res res = debug @@ fun _ ->
  print_endline @@ Yojson.Safe.to_string (res_to_yojson res)

let print_typ typ = debug @@ fun _ ->
  print_endline @@ Yojson.Safe.to_string (typ_to_yojson typ)

let print_type_ctx gamma = debug @@ fun _ ->
  let s =
   String.concat "\n, " @@
    List.map
      ( fun (name, (tau, bs)) ->
          name
            ^ " : "
            ^ (Yojson.Safe.to_string @@ typ_to_yojson tau)
            ^ " { "
            ^ (Yojson.Safe.to_string @@ bind_spec_to_yojson bs)
            ^ " }"
      )
      gamma
  in
    print_endline @@ "< " ^ s ^ "\n>"

let print_int n = debug @@ fun _ ->
  print_endline (string_of_int n)
