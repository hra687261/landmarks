(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

module Make(T: sig
    type landmark
    type node
    type profiling_state
    type landmark_key
    module W: Weak.S with type data = landmark_key
    module Stack: sig
      type ('a, 'arr) t
    end

    val landmark_key_of_landmark: landmark -> landmark_key
    val mk_landmark_key: string -> landmark -> landmark_key
    val landmark_of_landmark_key: landmark_key -> landmark
    val landmarks_of_key: W.t
    val init_landmark_root: unit -> node * landmark
    val dummy_profiling_state: node -> profiling_state
    val clear_landmark_key: node -> landmark_key -> unit
    val mk_profiling_stack: profiling_state -> (profiling_state, profiling_state array) Stack.t
  end) =
struct
  open T

  module Stack = T.Stack

  type t = unit

  let dummy_node, landmark_root = init_landmark_root ()

  let clear_cache () =
    W.iter (clear_landmark_key dummy_node) landmarks_of_key

  let landmark_root () = landmark_root
  let dummy_node () = dummy_node


  let add_landmark () landmark_key =
    W.add landmarks_of_key landmark_key

  let landmark_of_id () user_id =
    match
      W.find_opt landmarks_of_key (mk_landmark_key user_id (landmark_root ()))
    with
    | None -> None
    | Some landmark_key -> Some (landmark_of_landmark_key landmark_key)

  let find_or_add_landmark () id mk =
    match landmark_of_id () id with
    | None ->
        let lm: landmark = mk ~key:id () in
        add_landmark () (landmark_key_of_landmark lm);
        lm
    | Some lm -> lm

  let profiling_ref = ref false
  let profiling () = !profiling_ref
  let set_profiling () b =  profiling_ref := b
  let get_ds_landmark () l = l

  let node_id_ref = ref 0
  let get_node_id_ref () = !node_id_ref
  let set_node_id_ref () n = node_id_ref := n

  let allocated_nodes: T.node list ref = ref []
  let get_allocated_nodes () = !allocated_nodes
  let set_allocated_nodes () l = allocated_nodes := l

  let get_incr_node_id_ref () =
    let id = !node_id_ref in
    incr node_id_ref;
    id

  let current_root_node = ref (dummy_node ())
  let current_node_ref = ref !current_root_node

  let init ~reset_state:_ ~new_node ~stop_profiling:_ =
    current_root_node := new_node () (landmark_root ());
    current_node_ref := !current_root_node;
    fun () -> ()

  let get_current_root_node () = !current_root_node
  let get_current_node_ref () = !current_node_ref
  let set_current_root_node () node = current_root_node := node
  let set_current_node_ref () node = current_node_ref := node

  let cache_miss_ref = ref 0
  let get_cache_miss_ref () = !cache_miss_ref
  let set_cache_miss_ref () n = cache_miss_ref := n

  let profiling_stack: (profiling_state, profiling_state array) Stack.t =
    let dummy = dummy_profiling_state (dummy_node ()) in
    mk_profiling_stack dummy
  let incr_cache_miss_ref () = incr cache_miss_ref
  let get_profiling_stack () = profiling_stack

  let export ~export ~merge:_ ?(label = "") () =
    export () label

end
