let fresh_ident first_char gamma =
	let extract_number (ident : string) : int option =
    let ident_len =
      String.length ident
    in
      if ident_len > 0 && String.get ident 0 == first_char then
        ident
          |> String.sub 1 (ident_len - 1)
          |> int_of_string_opt
      else
        None
  in
  let fresh_number : int =
    gamma
      |> List.filter_map (fst >> extract_number)
      |> List2.maximum
      |> Option.map (fun n -> n + 1)
      |> Option.with_default 1
  in
    String.make 1 first_char ^ string_of_int fresh_number
