(** Function over cycles in a graph*)

type t

val graph_of_known_cipher : string -> string -> Graph.t
   
val cycles : Graph.t -> t

val to_paths : t -> Path.t list

val mem : Path.t -> t -> bool
