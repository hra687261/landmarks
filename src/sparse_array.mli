type 'a t

val dummy : unit -> 'a t

val make : 'a -> int -> 'a t

val reset : 'a t -> unit

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val values : 'a t -> 'a list
