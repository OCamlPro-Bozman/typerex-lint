val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val ( %> ) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val id : 'a -> 'a
val const : 'a -> 'b -> 'a
