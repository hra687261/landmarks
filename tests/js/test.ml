let call = Landmarks.register "fib"

let main = Landmarks.register "main"

let rec fib n =
  Landmarks.wrap call
    (fun n -> if n <= 1 then 1 else fib (n - 1) + fib (n - 2))
    n

let () =
  let open Landmarks in
  start_profiling ~profiling_options:{ Options.default with format = JSON } ();
  enter main;
  Printf.printf "%d\n%!" (fib 7);
  exit main;
  if Options.ongoing () then begin
    let open Graph in
    let cg = export () in
    let agg = aggregate_landmarks cg in
    let all_nodes = nodes agg in
    assert ((root cg).time > 0.);
    print_endline "\nLandmark reached:";
    all_nodes
    |> List.map (fun (node : Node.t) -> node.name)
    |> List.sort String.compare |> List.iter print_endline
  end
