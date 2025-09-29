let _ =
  let[@landmark] test1 x = x in
  (test1 "marc", test1 2)

let _ =
  let[@landmark] test2 (type t) (x : t) = x in
  (test2 "marc", test2 2)

let _ =
  let obj =
    object
      method test3 x = x [@@landmark]
    end
  in
  (obj#test3 "marc", obj#test3 2)

let () =
  let open Landmarks in
  if Options.ongoing () then begin
    let open Landmarks.Call_graph in
    let cg = export () in
    let agg = aggregate_landmarks cg in
    let all_nodes = nodes agg in
    print_endline "\nLandmark reached:";
    all_nodes
    |> List.map (fun { Landmarks.Cg_node.name; _ } -> name)
    |> List.sort compare |> List.iter print_endline
  end
