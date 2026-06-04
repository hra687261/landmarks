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

    val key_of_landmark: landmark ->  string
    val mk_landmark_key: string -> landmark -> landmark_key
    val landmark_of_landmark_key: landmark_key -> landmark
    val landmarks_of_key: W.t
    val init_landmark_root: unit -> node * landmark
    val dummy_profiling_state: node -> profiling_state
    val clear_landmark_key: node -> landmark_key -> unit
    val clone_landmark_key: node -> landmark_key -> landmark_key
    val mk_profiling_stack: profiling_state -> (profiling_state, profiling_state array) Stack.t
  end):
sig

  type t

  val get_ds_landmark: t -> T.landmark -> T.landmark

  val add_landmark: t -> T.landmark_key -> unit
  val landmark_of_id: t -> string -> T.landmark option
  val find_or_add_landmark:
    t ->
    string ->
    (key:string -> unit -> T.landmark) ->
    T.landmark

  val init:
    reset_state:(t -> unit) ->
    new_node:(t -> T.landmark -> T.node) ->
    stop_profiling:(t -> unit) ->
    unit ->
    t

  val landmark_root : t -> T.landmark
  val dummy_node : t -> T.node

  val profiling : t -> bool
  val set_profiling : t -> bool -> unit

  val get_node_id_ref : t -> int
  val set_node_id_ref : t -> int -> unit
  val get_incr_node_id_ref : t -> int
  val get_allocated_nodes : t -> T.node list
  val set_allocated_nodes : t -> T.node list -> unit

  val get_current_root_node : t -> T.node
  val set_current_root_node : t -> T.node -> unit
  val get_current_node_ref : t -> T.node
  val set_current_node_ref : t -> T.node -> unit
  val get_cache_miss_ref : t -> int
  val set_cache_miss_ref : t -> int -> unit

  val incr_cache_miss_ref : t -> unit

  val get_profiling_stack :
    t ->
    (T.profiling_state, T.profiling_state array) T.Stack.t

  val clear_cache : t -> unit
  val export :
    export:(t -> string -> Graph.graph) ->
    merge:(t -> T.node -> Graph.graph -> unit) ->
    ?label:string -> t -> Graph.graph

end
