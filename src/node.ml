type id = int

type kind =
  | Normal
  | Root
  | Counter
  | Sampler

let string_of_kind = function
  | Normal -> "normal"
  | Root -> "root"
  | Counter -> "counter"
  | Sampler -> "sampler"

type t =
  { id : id
  ; kind : kind
  ; landmark_id : string
  ; name : string
  ; location : string
  ; calls : int
  ; time : float
  ; children : id list
  ; sys_time : float
  ; allocated_bytes : int
  ; allocated_bytes_major : int
  ; distrib : floatarray
  }

let compare x y = Int.compare x.id y.id

let compare_time x y = Float.compare y.time x.time

let equal x y = Int.equal x.id y.id

let hash node = Hashtbl.hash node.id

let is_normal_or_root = function
  | { kind = Normal | Root; _ } -> true
  | _node -> false

let is_sampler = function { kind = Sampler; _ } -> true | _node -> false

let to_json
  { id
  ; kind
  ; landmark_id
  ; name
  ; location
  ; calls
  ; time
  ; children
  ; sys_time
  ; allocated_bytes
  ; allocated_bytes_major
  ; distrib
  } =
  let open Json in
  Map
    [ ("id", Int id)
    ; ("kind", String (string_of_kind kind))
    ; ("landmark_id", String landmark_id)
    ; ("name", String name)
    ; ("location", String location)
    ; ("calls", Int calls)
    ; ("time", Float time)
    ; ("children", List (List.map (fun x -> Int x) children))
    ; ("sys_time", Float sys_time)
    ; ("allocated_bytes", Int allocated_bytes)
    ; ("allocated_bytes_major", Int allocated_bytes_major)
    ; ( "distrib"
      , List (List.map (fun x -> Float x) (Float.Array.to_list distrib)) )
    ]
