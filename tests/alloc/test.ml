module L = Landmarks

let name = "zero_alloc"

let zero_alloc = L.register name

let () =
  L.start_profiling
    ~profiling_options:{ L.Options.default with allocated_bytes = true }
    ();
  L.enter zero_alloc;
  L.exit zero_alloc

let check_allocated_bytes () =
  let open L in
  let open Call_graph in
  let graph = L.export () in
  let node =
    root graph |> children graph
    |> List.filter (fun (node : Cg_node.t) -> node.name = name)
    |> List.hd
  in
  Printf.printf "Allocations:\n%d\n%d\n" node.allocated_bytes
    node.allocated_bytes_major

let () = check_allocated_bytes ()
