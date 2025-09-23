let x = (2 + 2) [@landmark "four"]

let () =
  begin [@landmark "seven"]
    ignore (3 + 4)
  end

let () =
  begin [@landmark "main"]
    let _ = [||] in
    ()
  end

let[@landmark "unit"] () =
  let[@landmark] main () = () in
  main ()

let () =
  let open Landmark in
  if profiling () then begin
    let open Landmark.Call_graph in
    let cg = export () in
    let agg = aggregate_landmarks cg in
    let all_nodes = nodes agg in
    print_endline "\nLandmark reached:";
    all_nodes
    |> List.map (fun { name; _ } -> name)
    |> List.sort compare |> List.iter print_endline
  end
