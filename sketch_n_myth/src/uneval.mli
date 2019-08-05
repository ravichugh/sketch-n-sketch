open Lang

val simplify :
  hole_ctx ->
  datatype_ctx ->
  res_constraints ->
  hole_constraints option

val uneval :
  hole_ctx ->
  datatype_ctx ->
  hole_filling ->
  res ->
  example ->
  hole_constraints option

val live_bidirectional_eval :
  hole_ctx ->
  datatype_ctx ->
  hole_filling ->
  res ->
  example ->
  hole_constraints option
