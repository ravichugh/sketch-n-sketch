exception Time_exceeded
exception Total_time_exceeded

let get () =
  Unix.gettimeofday ()

let check_cutoff ~max_time ~initial_time =
  if get () -. initial_time > max_time then
    raise_notrace Time_exceeded
  else
    ()

let initial_total_time =
  ref 0.0

let start_total () =
  initial_total_time := get ()

let total_elapsed () =
  get () -. !initial_total_time

let check_total_cutoff () =
  if total_elapsed () > Params.max_total_time then
    raise_notrace Total_time_exceeded
  else
    ()
