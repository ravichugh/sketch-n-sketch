let info s =
  if Params.log_info then
    print_endline @@
      "[INFO] " ^ s
  else
    ()

let warn s =
  if Params.log_warn then
    print_endline @@
      "[WARN] " ^ s
  else
    ()
