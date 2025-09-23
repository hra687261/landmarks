module A = struct
  type (_, _) kind =
    | Array : ('a, 'a array) kind
    | Float : (float, floatarray) kind

  let empty : type a arr. (a, arr) kind -> arr = function
    | Array -> [||]
    | Float -> Float.Array.create 0

  let make : type a arr. (a, arr) kind -> int -> a -> arr =
   fun kind n null ->
    match kind with
    | Array -> Array.make n null
    | Float -> Float.Array.make n null

  let length : type a arr. (a, arr) kind -> arr -> int =
   fun kind arr ->
    match kind with
    | Array -> Array.length arr
    | Float -> Float.Array.length arr

  let get : type a arr. (a, arr) kind -> arr -> int -> a =
   fun kind arr n ->
    match kind with Array -> Array.get arr n | Float -> Float.Array.get arr n

  let set : type a arr. (a, arr) kind -> arr -> int -> a -> unit =
   fun kind arr n ->
    match kind with Array -> Array.set arr n | Float -> Float.Array.set arr n

  let blit : type a arr.
    (a, arr) kind -> arr -> int -> arr -> int -> int -> unit =
   fun kind src srcpos dst dstpos n ->
    match kind with
    | Array -> Array.blit src srcpos dst dstpos n
    | Float -> Float.Array.blit src srcpos dst dstpos n
end

type ('a, 'arr) t =
  { kind : ('a, 'arr) A.kind
  ; mutable data : 'arr
  ; mutable size : int
  }

(* /!\ Dummy cannot be resized. *)
let dummy kind = { kind; data = A.empty kind; size = 0 }

let make kind null n = { kind; data = A.make kind (max 1 n) null; size = 0 }

let size { size; _ } = size

let resize ({ kind; size; data } as stack) =
  if size = A.length kind data then begin
    assert (size > 0);
    let new_length = (2 * (size + 1)) - 1 in
    stack.data <- A.make kind new_length (A.get kind data 0);
    A.blit kind data 0 stack.data 0 size
  end

let push stack x =
  resize stack;
  A.set stack.kind stack.data stack.size x;
  stack.size <- stack.size + 1

let pop stack =
  stack.size <- stack.size - 1;
  A.get stack.kind stack.data stack.size

let to_floatarray { data; size; _ } = Float.Array.sub data 0 size
