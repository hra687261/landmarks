let work    = Landmark.register "work"
let main_lm = Landmark.register "main"

let call_alloc_size = 65536

let do_work () =
  Landmark.enter work;
  ignore (Sys.opaque_identity (Bytes.create call_alloc_size));
  Landmark.exit work

let () =
  Landmark.start_profiling
    ~profiling_options:{Landmark.default_options with output = Silent} ();
  Landmark.enter main_lm;

  (* Main domain: 3 calls *)
  for _ = 1 to 3 do do_work () done;

  (* Two child domains: 5 calls each *)
  let d1 = Domain.spawn (fun () -> for _ = 1 to 5 do do_work () done) in
  let d2 = Domain.spawn (fun () -> for _ = 1 to 5 do do_work () done) in
  Domain.join d1;
  Domain.join d2;
  Landmark.exit main_lm;

  let open Landmark.Graph in
  let graph = Landmark.export () in
  let agg = aggregate_landmarks graph in
  let find name = List.find (fun n -> n.name = name) (nodes agg) in
  let work_node = find "work" in
  let main_node = find "main" in
  Printf.printf "work calls: %d\n" work_node.calls;
  Printf.printf "main calls: %d\n" main_node.calls;
  assert (
    work_node.allocated_bytes >= 13 * call_alloc_size &&
    work_node.allocated_bytes < 14 * call_alloc_size
  );
  Printf.printf "Each work call allocates: %d\n" call_alloc_size;
  Printf.printf "13 * %d <= allocated_bytes < 14 * %d : true\n" call_alloc_size call_alloc_size
