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
  end) =
struct
  open T

  module Stack = T.Stack

  type nodes = {
    mutable node_id_ref: int;
    mutable allocated_nodes: node list;
  }

  type t = {
    landmark_root: landmark;
    dummy_node : node;

    nodes: nodes;

    mutable profiling_ref : bool;
    mutable cache_miss_ref: int;
    profiling_stack: (profiling_state, profiling_state array) Stack.t;
    landmarks_of_key: W.t;

    mutable current_root_node : node;
    mutable current_node_ref : node;

    mutable child_states : t list;
    (* The states of child domains spawned by the main one *)
    mutable graph: Graph.graph;
    (* Used by child states to store their own graphs *)

    mutable registered: bool;
  }

  let init_nodes () = {
    node_id_ref = 0;
    allocated_nodes = [];
  }

  let get_incr_node_id_ref st =
    let id = st.nodes.node_id_ref in
    st.nodes.node_id_ref <- id + 1;
    id

  let clone_landmarks_of_key dummy_node w =
    let new_w = W.create (W.count w) in
    W.iter (
      fun landmark ->
        W.add new_w (clone_landmark_key dummy_node landmark)
    ) w;
    new_w

  let init ~reset_state ~new_node ~stop_profiling ~export =
    let init_state () =
      let dummy_node, landmark_root = init_landmark_root () in
      let nodes = init_nodes () in
      let st = {
        landmark_root;
        dummy_node;
        nodes;
        profiling_ref = false;
        cache_miss_ref = 0;
        profiling_stack = mk_profiling_stack (dummy_profiling_state dummy_node);
        landmarks_of_key = landmarks_of_key;
        child_states = [];
        graph = {nodes = [||]; label = ""; root = 0 };
        registered = false;
        (* Temprory *)
        current_root_node = dummy_node;
        current_node_ref = dummy_node;
      }
      in
      let root_node = new_node st landmark_root in
      { st with current_root_node = root_node; current_node_ref = root_node }
    in
    let state =
      Domain.DLS.new_key
        ~split_from_parent:(fun s ->
            let child_state = init_state () in
            let child_state =
              { child_state with
                profiling_ref = s.profiling_ref;
                landmarks_of_key =
                  clone_landmarks_of_key child_state.dummy_node s.landmarks_of_key
              }
            in
            s.child_states <- child_state :: s.child_states;
            child_state.profiling_ref <- s.profiling_ref;
            reset_state child_state;
            child_state
          )
        init_state
    in
    fun () ->
      let st = Domain.DLS.get state in
      if not st.registered && not (Domain.is_main_domain ()) then (
        Domain.at_exit (fun () ->
            stop_profiling st;
            st.graph <- export st ""
          );
        st.registered <- true;
      );
      st

  let landmarks_of_key_mutex = Mutex.create ()

  let add_landmark st landmark_key =
    if not (Domain.is_main_domain ()) then
      failwith "Child domains cannot register landmarks";
    Mutex.protect landmarks_of_key_mutex (fun () ->
        W.add st.landmarks_of_key landmark_key
      )

  let landmark_of_id st key =
    let lk_opt =
      Mutex.protect landmarks_of_key_mutex (fun () ->
          W.find_opt st.landmarks_of_key (mk_landmark_key key st.landmark_root)
        )
    in
    match lk_opt with
    | None -> None
    | Some lk -> Some (landmark_of_landmark_key lk)

  let find_or_add_landmark st key mk =
    Mutex.protect landmarks_of_key_mutex (fun () ->
        match
          W.find_opt st.landmarks_of_key (mk_landmark_key key st.landmark_root)
        with
        | Some lk -> landmark_of_landmark_key lk
        | None ->
            let new_landmark = mk ~key () in
            let lk = mk_landmark_key key new_landmark in
            W.add st.landmarks_of_key lk;
            new_landmark
      )

  let landmark_root st = st.landmark_root
  let dummy_node st = st.dummy_node

  let get_ds_landmark st lm =
    if Domain.is_main_domain () then lm else
      let key = key_of_landmark lm in
      let lm_key = mk_landmark_key key st.landmark_root in
      match W.find_opt st.landmarks_of_key lm_key with
      | Some lk -> landmark_of_landmark_key lk
      | None ->
          let new_lm_key =
            clone_landmark_key st.dummy_node (mk_landmark_key key lm)
          in
          W.add st.landmarks_of_key new_lm_key;
          landmark_of_landmark_key new_lm_key

  let clear_cache st: unit =
    W.iter (clear_landmark_key st.dummy_node) st.landmarks_of_key

  let profiling st = st.profiling_ref
  let set_profiling st b = st.profiling_ref <- b

  let get_node_id_ref st = st.nodes.node_id_ref
  let set_node_id_ref st n = st.nodes.node_id_ref <- n
  let get_allocated_nodes st = st.nodes.allocated_nodes
  let set_allocated_nodes st l = st.nodes.allocated_nodes <- l

  let get_current_root_node st = st.current_root_node
  let set_current_root_node st (node: node) =
    st.current_root_node <- node

  let get_current_node_ref st = st.current_node_ref
  let set_current_node_ref st (node: node) =
    st.current_node_ref <- node

  let get_cache_miss_ref st = st.cache_miss_ref
  let set_cache_miss_ref st n = st.cache_miss_ref <- n
  let incr_cache_miss_ref st = st.cache_miss_ref <- st.cache_miss_ref + 1
  let get_profiling_stack st = st.profiling_stack

  let rec merge_child_state_graphs ~merge state =
    List.iter (
      fun st ->
        merge_child_state_graphs ~merge st;
        merge st state.current_root_node st.graph
    ) state.child_states

  let export ~export ~merge ?(label = "") state =
    merge_child_state_graphs ~merge state;
    export state label


end


