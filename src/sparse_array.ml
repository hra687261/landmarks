type 'a t =
  { mutable keys : int array
  ; mutable data : 'a array
  ; mutable size : int
  }

(* /!\ Dummy cannot be resized. *)
let dummy () = { keys = [||]; data = [||]; size = 0 }

let make null n =
  let n = max n 1 in
  { keys = Array.make n 0; data = Array.make n null; size = 0 }

let reset sparse_array = sparse_array.size <- 0

let get t id =
  let { keys; data; size } = t in
  let min = ref 0 in
  let max = ref (size - 1) in
  while !min < !max do
    let middle = (!min + !max) / 2 in
    if Array.unsafe_get keys middle < id then min := middle + 1
    else max := middle
  done;
  let idx = !min in
  if idx = !max && Array.unsafe_get keys idx = id then Array.unsafe_get data idx
  else raise Not_found

let swap a i j =
  let t = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- t

let values { data; size; _ } =
  let result = ref [] in
  for k = 0 to size - 1 do
    result := data.(k) :: !result
  done;
  List.rev !result

let bubble { keys; data; size } =
  let pos = ref size in
  let key = keys.(size) in
  while
    let p = !pos in
    let q = p - 1 in
    if key < keys.(q) then begin
      swap keys p q;
      swap data p q;
      pos := q;
      q > 0
    end
    else false
  do
    ()
  done

let is_full { keys; size; _ } = Array.length keys = size

let resize ({ keys; data; size } as sparse_array) =
  if is_full sparse_array then begin
    assert (size > 0);
    let new_length = (2 * (size + 1)) - 1 in
    sparse_array.keys <- Array.make new_length 0;
    sparse_array.data <- Array.make new_length sparse_array.data.(0);
    Array.blit keys 0 sparse_array.keys 0 size;
    Array.blit data 0 sparse_array.data 0 size
  end

let set sparse_array id node =
  resize sparse_array;
  let size = sparse_array.size in
  sparse_array.keys.(size) <- id;
  sparse_array.data.(size) <- node;
  if size > 0 then bubble sparse_array;
  sparse_array.size <- sparse_array.size + 1
