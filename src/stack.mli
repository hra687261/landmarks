module A : sig
  type (_, _) kind =
    | Array : ('a, 'a array) kind
    | Float : (float, floatarray) kind
end

type ('a, 'arr) t

val dummy : ('a, 'arr) A.kind -> ('a, 'arr) t

val make : ('a, 'arr) A.kind -> 'a -> int -> ('a, 'arr) t

val push : ('a, 'arr) t -> 'a -> unit

val pop : ('a, 'arr) t -> 'a

val size : ('a, 'arr) t -> int

val to_floatarray : (float, floatarray) t -> Float.Array.t
