open Point

type circuit

val empty_circuits : point list -> circuit list
val update_circuits : circuit list -> point * point -> circuit list
val get_circuits_list : circuit list -> point list list
