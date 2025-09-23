(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

module Cg_node = Cg_node
module Call_graph = Call_graph
module Options = Options

external clock : unit -> (Int64.t[@unboxed])
  = "caml_highres_clock" "caml_highres_clock_native"
[@@noalloc]

(* Alternative implementation of Gc.allocated_bytes which does not allocate *)
external allocated_bytes : unit -> (Int64.t[@unboxed])
  = "allocated_bytes" "allocated_bytes_native"
[@@noalloc]

external allocated_bytes_major : unit -> (Int64.t[@unboxed])
  = "allocated_bytes_major" "allocated_bytes_major_native"
[@@noalloc]

let allocated_bytes () = Int64.to_int (allocated_bytes ())

let allocated_bytes_major () = Int64.to_int (allocated_bytes_major ())

exception LandmarkFailure of string

type floats =
  { mutable time : float
  ; mutable allocated_bytes : int
  ; mutable allocated_bytes_stamp : int
  ; mutable allocated_bytes_major : int
  ; mutable allocated_bytes_major_stamp : int
  ; mutable sys_time : float
  ; mutable sys_timestamp : float
  }

type landmark =
  { id : int
  ; key : landmark_key
  ; kind : Cg_node.kind
  ; name : string
  ; location : string
  ; mutable last_parent : node
  ; mutable last_son : node
  ; mutable last_self : node
  }

and node =
  { landmark : landmark
  ; id : int
  ; children : node Sparse_array.t
  ; fathers : (node, node array) Stack.t
  ; mutable calls : int
  ; mutable recursive_calls : int
  ; mutable timestamp : Int64.t
  ; distrib : (float, floatarray) Stack.t
  ; floats : floats
  }

and landmark_key =
  { key : string
  ; landmark : landmark
  }

type counter = landmark

type sampler = landmark

let new_floats () =
  { time = 0.0
  ; allocated_bytes = 0
  ; allocated_bytes_stamp = 0
  ; allocated_bytes_major = 0
  ; allocated_bytes_major_stamp = 0
  ; sys_time = 0.0
  ; sys_timestamp = 0.0
  }

let rec landmark_root =
  { kind = Cg_node.Root
  ; id = 0
  ; name = "ROOT"
  ; location = __FILE__
  ; key = { key = ""; landmark = landmark_root }
  ; last_parent = dummy_node
  ; last_son = dummy_node
  ; last_self = dummy_node
  }

and dummy_node =
  { landmark = landmark_root
  ; id = 0
  ; children = Sparse_array.dummy ()
  ; fathers = Stack.dummy Array
  ; floats = new_floats ()
  ; calls = 0
  ; recursive_calls = 0
  ; distrib = Stack.dummy Float
  ; timestamp = Int64.zero
  }

let dummy_key = { key = ""; landmark = landmark_root }

(** REGISTERING **)

let last_landmark_id = ref 1

module W = Weak.Make (struct
  type t = landmark_key

  let equal (x : landmark_key) (y : landmark_key) = String.equal x.key y.key

  let hash (x : landmark_key) = Hashtbl.hash x.key
end)

let landmarks_of_key = W.create 17

let iter_registered_landmarks f =
  W.iter (fun { landmark; _ } -> f landmark) landmarks_of_key

let landmark_of_id user_id =
  match W.find_opt landmarks_of_key { dummy_key with key = user_id } with
  | None -> None
  | Some { landmark; _ } -> Some landmark

let new_landmark ~key:key_string ~name ~location ~kind () =
  let id = !last_landmark_id in
  incr last_landmark_id;
  let rec res =
    { id
    ; name
    ; location
    ; kind
    ; key
    ; last_parent = dummy_node
    ; last_self = dummy_node
    ; last_son = dummy_node
    }
  and key = { landmark = res; key = key_string } in
  W.add landmarks_of_key key;
  res

let node_id_ref = ref 0

let allocated_nodes = ref []

let new_node landmark =
  if Options.with_debug () then
    Printf.eprintf "[Profiling] Allocating new node for %s...\n%!" landmark.name;
  let id = !node_id_ref in
  incr node_id_ref;
  let node =
    { landmark
    ; id
    ; fathers = Stack.make Array dummy_node 1
    ; distrib = Stack.make Float 0.0 0
    ; children = Sparse_array.make dummy_node 7
    ; calls = 0
    ; recursive_calls = 0
    ; timestamp = Int64.zero
    ; floats = new_floats ()
    }
  in
  allocated_nodes := node :: !allocated_nodes;
  node

let current_root_node = ref (new_node landmark_root)

let landmark_of_node ({ landmark_id = key; name; location; kind; _ } : Cg_node.t)
    =
  match landmark_of_id key with
  | None -> new_landmark ~key ~name ~kind ~location ()
  | Some landmark -> landmark

let register_generic ~id ~name ~location ~kind () =
  let landmark = new_landmark ~key:id ~name ~location ~kind () in
  if Options.with_debug () then
    Printf.eprintf "[Profiling] registering(%s)\n%!" name;
  landmark

let register_generic ~id ~location kind name =
  match landmark_of_id id with
  | None -> register_generic ~id ~name ~location ~kind ()
  | Some lm -> lm

let register_generic ?id ?location kind name =
  let location =
    match location with
    | Some name -> name
    | None -> (
      let callstack = Printexc.get_callstack 5 in
      let backtrace_slots = Printexc.backtrace_slots callstack in
      match backtrace_slots with
      | Some [||] | None -> "unknown"
      | Some slots -> (
        let last_slot = slots.(Array.length slots - 1) in
        match Printexc.Slot.location last_slot with
        | Some { Printexc.filename; line_number; _ } ->
          Printf.sprintf "%s:%d" filename line_number
        | None -> "internal" ) )
  in
  let id = match id with Some key -> key | None -> name ^ "-" ^ location in
  register_generic ~id ~location kind name

let register ?id ?location name =
  register_generic ?id ?location Cg_node.Normal name

let register_counter name = register_generic Cg_node.Counter name

let register_sampler name = register_generic Cg_node.Sampler name

let current_node_ref = ref !current_root_node

let cache_miss_ref = ref 0

let stamp_root () =
  !current_root_node.timestamp <- clock ();
  if Options.with_allocated_bytes () then begin
    !current_root_node.floats.allocated_bytes <- allocated_bytes ();
    !current_root_node.floats.allocated_bytes_major <- allocated_bytes_major ()
  end;
  if Options.with_sys_time () then
    !current_root_node.floats.sys_time <- Sys.time ()

let clear_cache () =
  let reset_landmark landmark =
    landmark.last_son <- dummy_node;
    landmark.last_parent <- dummy_node;
    landmark.last_self <- dummy_node
  in
  iter_registered_landmarks reset_landmark

type node_info =
  { node : node
  ; recursive : bool
  }

type profiling_state =
  { root : node
  ; nodes : node_info list
  ; nodes_len : int
  ; current : node
  ; cache_miss : int
  }

let profiling_stack =
  let dummy =
    { root = dummy_node
    ; current = dummy_node
    ; nodes = [ { node = dummy_node; recursive = false } ]
    ; cache_miss = 0
    ; nodes_len = 1
    }
  in
  Stack.make Array dummy 7

let reset () =
  if Options.with_debug () then Printf.eprintf "[Profiling] resetting ...\n%!";
  (* reset dummy_node *)
  let floats = !current_root_node.floats in
  floats.time <- 0.0;
  floats.allocated_bytes <- 0;
  floats.sys_time <- 0.0;
  !current_root_node.calls <- 0;
  !current_root_node.recursive_calls <- 0;
  stamp_root ();
  Sparse_array.reset !current_root_node.children;
  allocated_nodes := [ !current_root_node ];
  current_node_ref := !current_root_node;
  cache_miss_ref := 0;
  clear_cache ();
  node_id_ref := 1

let () = reset ()

let push_profiling_state () =
  if Options.with_debug () then
    Printf.eprintf "[Profiling] Push profiling state ....\n%!";
  let state =
    let node_info node =
      let recursive = node.landmark.last_self == node in
      { node; recursive }
    in
    { root = !current_root_node
    ; nodes = List.map node_info !allocated_nodes
    ; nodes_len = !node_id_ref
    ; current = !current_node_ref
    ; cache_miss = !cache_miss_ref
    }
  in
  clear_cache ();
  current_root_node := new_node landmark_root;
  current_node_ref := !current_root_node;
  cache_miss_ref := 0;
  allocated_nodes := [ !current_root_node ];
  node_id_ref := 1;
  reset ();
  Stack.push profiling_stack state

let pop_profiling_state () =
  if Stack.size profiling_stack > 0 then (
    let { root; nodes; nodes_len; current; cache_miss } =
      Stack.pop profiling_stack
    in
    current_root_node := root;
    current_node_ref := current;
    cache_miss_ref := cache_miss;
    allocated_nodes :=
      List.map
        (fun { node; recursive } ->
          if recursive then node.landmark.last_self <- node;
          node )
        nodes;
    node_id_ref := nodes_len )

let unroll_until node =
  while
    let current_node = !current_node_ref in
    current_node != node
    && Stack.size current_node.fathers > 0
    &&
    ( current_node_ref := Stack.pop current_node.fathers;
      true )
  do
    ()
  done

let landmark_failure msg =
  unroll_until !current_root_node;
  if !current_node_ref != !current_root_node then reset ();
  if Options.with_debug () then (
    Printf.eprintf "Landmark error: %s\n%!" msg;
    Stdlib.exit 2 )
  else raise (LandmarkFailure msg)

let get_entering_node ({ id; _ } as landmark) =
  let current_node = !current_node_ref in
  (* Read the "cache". *)
  if current_node == landmark.last_parent && landmark.last_son != dummy_node
  then landmark.last_son
  else begin
    incr cache_miss_ref;
    (* We fetch the son or create it. *)
    let children = current_node.children in
    let son =
      try Sparse_array.get children id
      with Not_found ->
        let son = new_node landmark in
        Sparse_array.set current_node.children id son;
        son
    in
    (* Fill the "cache". *)
    landmark.last_parent <- current_node;
    landmark.last_son <- son;
    son
  end

let get_exiting_node current_node =
  if Stack.size current_node.fathers = 0 then landmark_failure "Stack underflow"
  else Stack.pop current_node.fathers

let increment ?(times = 1) counter =
  let node = get_entering_node counter in
  node.calls <- node.calls + times

let increment ?times counter =
  if Options.ongoing () then increment ?times counter

let sample sampler x =
  let node = get_entering_node sampler in
  node.calls <- node.calls + 1;
  Stack.push node.distrib x

let sample sampler x = if Options.ongoing () then sample sampler x

let enter landmark =
  if Options.with_debug () then
    Printf.eprintf "[Profiling] enter%s(%s)\n%!"
      (if landmark.last_self != dummy_node then " recursive " else "")
      landmark.name;

  if landmark.last_self == dummy_node || Options.recursive () then begin
    let node = get_entering_node landmark in
    node.calls <- node.calls + 1;
    Stack.push node.fathers !current_node_ref;
    current_node_ref := node;
    landmark.last_self <- node;
    node.timestamp <- clock ();
    if Options.with_allocated_bytes () then begin
      node.floats.allocated_bytes_stamp <- allocated_bytes ();
      node.floats.allocated_bytes_major_stamp <- allocated_bytes_major ()
    end;
    if Options.with_sys_time () then node.floats.sys_timestamp <- Sys.time ()
  end
  else begin
    let last_self = landmark.last_self in
    last_self.recursive_calls <- last_self.recursive_calls + 1;
    last_self.calls <- last_self.calls + 1
  end

let mismatch_recovering landmark current_node =
  let expected_landmark = current_node.landmark in
  if expected_landmark != landmark then begin
    let msg =
      Printf.sprintf
        "landmark failure when closing '%s' (%s), expecting '%s' (%s)."
        landmark.name landmark.location expected_landmark.name
        expected_landmark.location
    in
    Printf.eprintf "Warning: %s\n%!" msg;
    unroll_until landmark.last_self;
    if landmark != !current_node_ref.landmark then begin
      reset ();
      landmark_failure ("unable to recover from " ^ msg)
    end
  end

let aggregate_stat_for current_node =
  let floats = current_node.floats in
  floats.time <-
    (floats.time +. Int64.(to_float (sub (clock ()) current_node.timestamp)));
  if Options.with_allocated_bytes () then begin
    floats.allocated_bytes <-
      floats.allocated_bytes
      + (allocated_bytes () - floats.allocated_bytes_stamp);
    floats.allocated_bytes_major <-
      floats.allocated_bytes_major
      + (allocated_bytes_major () - floats.allocated_bytes_major_stamp)
  end;
  if Options.with_sys_time () then
    floats.sys_time <- floats.sys_time +. (Sys.time () -. floats.sys_timestamp)

let exit landmark =
  if Options.with_debug () then
    Printf.eprintf "[Profiling] exit%s(%s)\n%!"
      (if landmark.last_self != !current_node_ref then " recursive " else "")
      landmark.name;
  let current_node = !current_node_ref in
  let last_self = landmark.last_self in
  if last_self.recursive_calls = 0 || Options.recursive () then begin
    mismatch_recovering landmark current_node;
    if Stack.size current_node.fathers = 1 then begin
      landmark.last_self <- dummy_node;
      aggregate_stat_for current_node
    end;
    current_node_ref := get_exiting_node current_node
  end
  else if not @@ Options.recursive () then
    last_self.recursive_calls <- last_self.recursive_calls - 1

(* These two functions should be inlined. *)
let enter landmark = if Options.ongoing () then enter landmark

let exit landmark = if Options.ongoing () then exit landmark

(** HELPERS **)

let wrap node f x =
  enter node;
  try
    let res = f x in
    exit node;
    res
  with
  | LandmarkFailure _ as e -> raise e
  | e ->
    exit node;
    raise e

let unsafe_wrap node f x =
  enter node;
  let res = f x in
  exit node;
  res

let start_profiling ?(profiling_options = Options.default) () =
  if Options.ongoing () then
    failwith "In profiling: it is not allowed to nest profilings.";
  Options.set_current profiling_options;
  if Options.with_debug () then
    Printf.eprintf "[Profiling] Start profiling %s...\n%!"
      ( match (Options.with_allocated_bytes (), Options.with_sys_time ()) with
      | true, true -> "with garbage collection statistics and system time"
      | true, false -> "with garbage collection statistics"
      | false, true -> "with system time"
      | false, false -> "" );
  Options.set_ongoing true

let rec exit_until_root () =
  if !current_node_ref != !current_root_node then begin
    let landmark = !current_node_ref.landmark in
    exit landmark;
    exit_until_root ()
  end

let stop_profiling () =
  if not @@ Options.ongoing () then
    failwith "In profiling: cannot stop since profiling is not on-going";
  exit_until_root ();
  let current_node = !current_node_ref in
  assert (current_node == !current_root_node);
  aggregate_stat_for current_node;
  if Options.with_debug () then Printf.eprintf "[Profiling] Stop profiling.\n%!";
  Options.set_ongoing false

(** EXPORTING / IMPORTING SLAVE PROFILINGS **)

let array_list_map f l =
  let size = List.length l in
  match l with
  | [] -> [||]
  | hd :: tl ->
    let res = Array.make size (f hd) in
    List.iteri (fun k x -> res.(k + 1) <- f x) tl;
    res

let export ?(label = "") () =
  let export_node { landmark; id; calls; floats; children; distrib; _ } =
    let { key = { key = landmark_id; _ }; name; location; kind; _ } =
      landmark
    in
    let { time; allocated_bytes; allocated_bytes_major; sys_time; _ } =
      floats
    in
    let children =
      List.map (fun ({ id; _ } : node) -> id) (Sparse_array.values children)
    in
    { Cg_node.landmark_id
    ; id
    ; name
    ; location
    ; calls
    ; time
    ; kind
    ; allocated_bytes
    ; allocated_bytes_major
    ; sys_time
    ; children
    ; distrib = Stack.to_floatarray distrib
    }
  in
  if Options.ongoing () then begin
    aggregate_stat_for !current_root_node;
    stamp_root ()
  end;
  let all_nodes = List.rev !allocated_nodes in
  let nodes = array_list_map export_node all_nodes in
  { Call_graph.nodes; label; root = 0 }

let export_and_reset ?label () =
  let profiling = Options.ongoing () in
  if profiling then stop_profiling ();
  let res = export ?label () in
  reset ();
  if profiling then start_profiling ();
  res

let rec merge_branch node graph (imported : Cg_node.t) =
  let floats = node.floats in
  floats.time <- imported.time +. floats.time;
  floats.sys_time <- imported.sys_time +. floats.sys_time;
  floats.allocated_bytes <- imported.allocated_bytes + floats.allocated_bytes;
  floats.allocated_bytes_major <-
    imported.allocated_bytes_major + floats.allocated_bytes_major;
  node.calls <- imported.calls + node.calls;
  Float.Array.iter (Stack.push node.distrib) imported.distrib;

  let children = Call_graph.children graph imported in
  List.iter
    (fun (imported_son : Cg_node.t) ->
      let landmark = landmark_of_node imported_son in
      match Sparse_array.get node.children landmark.id with
      | exception Not_found -> new_branch node graph imported_son
      | son -> merge_branch son graph imported_son )
    children

and new_branch parent graph (imported : Cg_node.t) =
  let landmark = landmark_of_node imported in
  let node = new_node landmark in
  node.calls <- imported.calls;
  let floats = node.floats in
  floats.time <- imported.time;
  floats.allocated_bytes <- imported.allocated_bytes;
  floats.sys_time <- imported.sys_time;
  Float.Array.iter (Stack.push node.distrib) imported.distrib;
  Sparse_array.set parent.children landmark.id node;
  List.iter (new_branch node graph) (Call_graph.children graph imported)

let merge (graph : Call_graph.t) =
  if Options.with_debug () then
    Printf.eprintf "[Profiling] merging foreign graph\n%!";
  merge_branch !current_root_node graph (Call_graph.root graph)

let exit_hook () =
  if Options.with_debug () then Printf.eprintf "[Profiling] exit_hook\n%!";
  if Options.ongoing () then begin
    stop_profiling ();
    let label = String.concat " " (Array.to_list Sys.argv) in
    let cg = export ~label () in
    match (Options.output (), Options.format ()) with
    | Silent, _ -> ()
    | Channel out, Textual { threshold } -> Call_graph.output ~threshold out cg
    | Channel out, JSON -> Call_graph.output_json out cg
    | Temporary temp_dir, format ->
      let tmp_file, oc =
        Filename.open_temp_file ?temp_dir "profile_at_exit" ".tmp"
      in
      Printf.eprintf "[Profiling] Dumping profiling information in file '%s'.\n"
        tmp_file;
      flush stdout;
      ( match format with
      | Textual { threshold } -> Call_graph.output ~threshold oc cg
      | JSON -> Call_graph.output_json oc cg );
      close_out oc
  end

let () = Stdlib.at_exit exit_hook

let () =
  match Sys.getenv "OCAML_LANDMARKS" with
  | exception Not_found -> ()
  | str -> (
    try start_profiling ~profiling_options:(Options.parse_env str) ()
    with Exit -> () )

external raise : exn -> 'a = "%raise"
