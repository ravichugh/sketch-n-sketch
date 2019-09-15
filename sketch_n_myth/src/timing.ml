exception Time_exceeded

let get () =
  Unix.gettimeofday ()

let check_cutoff ~max_time ~initial_time =
  if get () -. initial_time > max_time then
    raise_notrace Time_exceeded
  else
    ()
