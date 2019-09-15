exception Time_exceeded

let max_time =
  10.0

let get () =
  Unix.gettimeofday ()

let check_cutoff initial_time =
  if get () -. initial_time > max_time then
    raise_notrace Time_exceeded
  else
    ()
