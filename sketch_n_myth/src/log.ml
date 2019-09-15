let enable_info =
  true

let enable_warn =
  true

let info s =
  if enable_info then
    print_endline @@
      "[INFO] " ^ s
  else
    ()

let warn s =
  if enable_warn then
    print_endline @@
      "[WARN] " ^ s
  else
    ()
