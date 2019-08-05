open Lang

val simplify : res_constraints -> hole_constraints option

val uneval :
  hole_ctx ->
  datatype_ctx ->
  hole_filling ->
  res ->
  example ->
  hole_constraints option
