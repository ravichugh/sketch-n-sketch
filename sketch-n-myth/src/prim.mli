open Lang

val val_typ : prim_val -> prim_typ
val op_typ : prim_op -> typ

val val_equal : prim_val -> prim_val -> bool
val typ_equal : prim_typ -> prim_typ -> bool
