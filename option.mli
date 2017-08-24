val is_none : 'a option -> bool

val is_some : 'a option -> bool

val value_exn : 'a option -> 'a

val value : 'a option -> default:'a -> 'a

val value_map : 'a option -> default:'b -> f:('a -> 'b) -> 'b

val iter : 'a option -> f:('a -> unit) -> unit
