(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

(* TODO: remove me *)
module Hashtbl = struct
  include Hashtbl

  let find tbl k =
    match Hashtbl.find_opt tbl k with None -> raise Not_found | Some v -> v
end

(* TODO: remove me *)
let ( ^ ) l r = String.concat "" [ l; r ]

module SetNode = Set.Make (Node)

(* TODO: remove me *)
module HashNode = struct
  include Hashtbl.Make (Node)

  let find tbl k =
    match find_opt tbl k with None -> raise Not_found | Some v -> v
end

let group_proj f l =
  let tbl = Hashtbl.create (List.length l) in
  List.iter
    (fun x ->
      let key = f x in
      match Hashtbl.find_opt tbl key with
      | None -> Hashtbl.replace tbl key [ x ]
      | Some l -> Hashtbl.replace tbl key (x :: l) )
    l;
  Hashtbl.fold (fun _ value acc -> value :: acc) tbl []

let base_name s =
  match String.rindex_opt s '.' with None -> s | Some k -> String.sub s 0 k

module StringSet = Set.Make (String)

let group_by ~equals l =
  let rec aux cur stk acc = function
    | [] -> List.rev (stk :: acc)
    | hd :: tl when equals cur hd -> aux cur (hd :: stk) acc tl
    | hd :: tl -> aux hd [ hd ] (List.rev stk :: acc) tl
  in
  match l with [] -> [] | hd :: tl -> aux hd [ hd ] [] tl

let rec choose f = function
  | [] -> []
  | hd :: tl -> (
    match f hd with Some x -> x :: choose f tl | None -> choose f tl )

let duplicated_elements l =
  List.sort String.compare l
  |> group_by ~equals:String.equal
  |> choose (function x :: _ :: _ -> Some x | _ -> None)

type t =
  { nodes : Node.t array
  ; label : string
  ; root : Node.id
  }

let of_nodes ?(label = "") ?(root = 0) nodes =
  { nodes = Array.of_list nodes; label; root }

let children { nodes; _ } (node : Node.t) =
  List.map (fun k -> nodes.(k)) node.children |> List.sort Node.compare_time

let nodes { nodes; _ } = Array.to_list nodes

let root { nodes; root; _ } = nodes.(root)

let subgraph graph (node : Node.t) = { graph with root = node.id }

let prune graph =
  let visited_table = HashNode.create 17 in
  let rec aux vertex =
    let visited = HashNode.mem visited_table vertex in
    if not visited then begin
      HashNode.add visited_table vertex ();
      List.iter aux (children graph vertex)
    end
  in
  let node = root graph in
  aux node;
  let nodes =
    let l = ref [ node ] in
    HashNode.iter
      (fun n () -> if n.id <> node.id then l := n :: !l)
      visited_table;
    List.rev !l
  in
  let translator = Hashtbl.create 17 in
  List.iteri
    (fun k (n : Node.t) ->
      assert (not (Hashtbl.mem translator n.id));
      Hashtbl.add translator n.id k )
    nodes;
  let translate (node : Node.t) =
    try
      let id = Hashtbl.find translator node.id in
      let children = List.map (Hashtbl.find translator) node.children in
      { node with id; children }
    with Not_found -> assert false
  in
  let nodes = Array.of_list (List.map translate nodes) in
  { nodes; label = ""; root = 0 }

let path_dfs f g graph =
  let visited_table = HashNode.create 17 in
  let rec aux ancestors_set ancestors vertex =
    let visited = HashNode.mem visited_table vertex in
    if not visited then HashNode.add visited_table vertex ();
    if SetNode.mem vertex ancestors_set then g ancestors vertex
    else begin
      f visited ancestors vertex;
      List.iter
        (aux (SetNode.add vertex ancestors_set) (vertex :: ancestors))
        (children graph vertex)
    end
  in
  aux SetNode.empty [] (root graph)

let dfs f g graph =
  let visited_table = HashNode.create 17 in
  let rec aux ancestors_set ancestors vertex =
    let visited = HashNode.mem visited_table vertex in
    if visited then begin
      g ancestors vertex
    end
    else begin
      HashNode.add visited_table vertex ();
      if f ancestors vertex then
        List.iter
          (aux (SetNode.add vertex ancestors_set) (vertex :: ancestors))
          (children graph vertex)
    end
  in
  aux SetNode.empty [] (root graph)

let depth graph =
  let result = HashNode.create 17 in
  dfs
    (fun ancestor node ->
      let depth =
        match ancestor with
        | [] -> 0
        | father :: _ -> HashNode.find result father + 1
      in
      begin
        match HashNode.find_opt result node with
        | None -> HashNode.replace result node depth
        | Some old_depth -> HashNode.replace result node (min old_depth depth)
      end;
      true )
    (fun _ _ -> ())
    graph;
  fun node -> HashNode.find result node

let shallow_ancestor graph =
  let result = HashNode.create 17 in
  dfs
    (fun ancestor node ->
      let sa =
        match ancestor with
        | [] -> node
        | [ root ] -> root
        | [ father; _ ] -> father
        | father :: _ -> HashNode.find result father
      in
      HashNode.replace result node sa;
      true )
    (fun _ _ -> ())
    graph;
  fun node -> HashNode.find result node

let total_number_of_calls graph =
  List.fold_left (fun acc (node : Node.t) -> acc + node.calls) 0 (nodes graph)

let aggregate_landmarks graph =
  let { nodes; label; root } = prune graph in
  let root_id = nodes.(root).landmark_id in
  let group_nodes =
    group_proj
      (fun ({ landmark_id; _ } : Node.t) -> landmark_id)
      (Array.to_list nodes)
  in
  let translator = Hashtbl.create 17 in
  List.iteri
    (fun i -> function
      | [] -> assert false
      | (hd : Node.t) :: _tl -> Hashtbl.replace translator hd.landmark_id i )
    group_nodes;
  let aggregate_nodes (l : Node.t list) =
    match l with
    | [] -> assert false
    | hd :: tl ->
      let id =
        match Hashtbl.find_opt translator hd.landmark_id with
        | None -> assert false
        | Some id -> id
      in
      let time =
        List.fold_left (fun acc (node : Node.t) -> acc +. node.time) hd.time tl
      in
      let calls =
        List.fold_left (fun acc (node : Node.t) -> acc + node.calls) hd.calls tl
      in
      let sys_time =
        List.fold_left
          (fun acc (node : Node.t) -> acc +. node.sys_time)
          hd.sys_time tl
      in
      let allocated_bytes, allocated_bytes_major =
        List.fold_left
          (fun (ab, abm) (node : Node.t) ->
            (ab + node.allocated_bytes, abm + node.allocated_bytes_major) )
          (hd.allocated_bytes, hd.allocated_bytes_major)
          tl
      in
      let children =
        let lm_ids_of_children (node : Node.t) =
          StringSet.of_list
            (List.map (fun id -> nodes.(id).landmark_id) node.children)
        in
        try
          List.fold_left
            (fun acc node -> StringSet.union acc (lm_ids_of_children node))
            StringSet.empty l
          |> StringSet.elements
          |> List.map (Hashtbl.find translator)
        with Not_found -> assert false
      in
      { hd with
        id
      ; time
      ; calls
      ; sys_time
      ; allocated_bytes
      ; allocated_bytes_major
      ; children
      }
  in
  let root =
    match Hashtbl.find_opt translator root_id with
    | None -> assert false
    | Some root -> root
  in
  let nodes = Array.of_list (List.map aggregate_nodes group_nodes) in

  { nodes
  ; label =
      (if String.equal "" label then "aggretated" else label ^ " (aggregated)")
  ; root
  }

let intensity ?(proj = fun (node : Node.t) -> node.time) graph =
  let sa = shallow_ancestor graph in
  fun node ->
    let not_accounted =
      List.fold_left
        (fun t node -> t -. proj node)
        (proj node) (children graph node)
    in
    let reference = proj (sa node) in
    if Float.equal reference 0.0 then 0.0 else not_accounted /. reference

let color graph =
  let intensity = intensity graph in
  let red s = "\027[0;31m" ^ s ^ "\027[0m" in
  let bold_red s = "\027[1;31m" ^ s ^ "\027[0m" in
  let yellow s = "\027[0;33m" ^ s ^ "\027[0m" in
  let white s = s in
  let bold_white s = "\027[1;37m" ^ s ^ "\027[0m" in
  let cyan s = "\027[1;36m" ^ s ^ "\027[0m" in
  let purple s = "\027[1;35m" ^ s ^ "\027[0m" in
  let white_bg s = "\027[1;47m" ^ s ^ "\027[0m" in
  let black s = "\027[1;30m" ^ s ^ "\027[0m" in
  fun (node : Node.t) ->
    match node.kind with
    | Normal -> begin
      let x = intensity node in
      if Float.compare x 0.0 > 0 then
        if Float.compare x 0.15 > 0 then bold_red
        else if Float.compare x 0.05 > 0 then red
        else if Float.compare x 0.01 > 0 then yellow
        else white
      else bold_white
    end
    | Counter -> cyan
    | Sampler -> purple
    | Root -> fun x -> white_bg (black x)

let label graph =
  let nodes = group_proj (fun (node : Node.t) -> node.location) (nodes graph) in
  let names =
    List.concat_map
      (fun l ->
        List.sort_uniq String.compare
          (List.map (fun (node : Node.t) -> node.name) l) )
      nodes
  in
  let needs_location = StringSet.of_list (duplicated_elements names) in
  fun (node : Node.t) ->
    if StringSet.mem node.name needs_location then
      let location = base_name node.location in
      Printf.sprintf "%s (%s)" node.name location
    else node.name

let output ?(threshold = 1.0) oc graph =
  Printf.fprintf oc "Call graph%s:\n-----------%s\n%!"
    (if graph.label = "" then "" else " '" ^ graph.label ^ "'")
    ( if graph.label = "" then ""
      else String.make (String.length graph.label + 3) '-' );
  let label = label graph in
  let color = color graph in
  let human x =
    if x < 1e3 then (x, " ")
    else if x < 1e6 then (x /. 1e3, "K")
    else if x < 1e9 then (x /. 1e6, "M")
    else (x /. 1e9, "G")
  in
  let spaces depth =
    let bytes = Bytes.make ((4 * depth) + 1) ' ' in
    for k = 1 to depth - 1 do
      Bytes.set bytes (4 * k) '|'
    done;
    Bytes.set bytes (4 * depth) '-';
    Bytes.to_string bytes
  in
  let digits_of_call =
    int_of_float
    @@ 1.
       +. log10
            ( List.map (fun (node : Node.t) -> node.calls) (nodes graph)
            |> List.fold_left max 1 |> float_of_int )
  in
  let regular_call ancestors (node : Node.t) =
    match ancestors with
    | [] -> true
    | (father : Node.t) :: _ ->
      let depth = List.length ancestors in
      let spaces = spaces depth in
      let this_time, father_time = (node.time, father.time) in
      if node.calls > 0 then (
        if father_time > 0.0 then
          let percent = 100.0 *. this_time /. father_time in
          let this_time, unit = human this_time in
          if percent >= threshold then begin
            Printf.fprintf oc "%s\n%!"
              (Printf.sprintf "[ %7.2f%1s cycles in %*d calls ] %s %5.2f%% : %s"
                 this_time unit digits_of_call node.calls spaces percent
                 (color node (label node)) );
            true
          end
          else false
        else
          let this_time, unit = human this_time in
          Printf.fprintf oc "%s\n%!"
            (Printf.sprintf "[ %7.2f%1s  cycles in %7d calls ] %s * %s"
               this_time unit node.calls spaces
               (color node (label node)) );
          false )
      else false
  in
  let recursive_call ancestors node =
    let depth = List.length ancestors in
    let spaces = spaces depth in
    Printf.fprintf oc "%37s%s*** RECURSIVE CALL TO '%s' ***\n%!" "!!!!" spaces
      (label node)
  in
  dfs regular_call recursive_call graph;
  let aggregated_graph = aggregate_landmarks graph in
  let all_nodes = List.sort Node.compare_time (nodes aggregated_graph) in
  let normal_nodes = List.filter Node.is_normal_or_root all_nodes in
  let sample_nodes = List.filter Node.is_sampler all_nodes in
  let profile_with_sys_time =
    List.exists (fun (node : Node.t) -> node.sys_time <> 0.0) normal_nodes
  in
  let profile_with_allocated_bytes =
    List.exists (fun (node : Node.t) -> node.allocated_bytes <> 0) normal_nodes
  in
  let optional_headers =
    match (profile_with_sys_time, profile_with_allocated_bytes) with
    | true, true ->
      Printf.sprintf "; %8s; %8s; %8s" "Sys time" "Allocated bytes"
        "Allocated bytes major"
    | true, false -> Printf.sprintf "; %8s" "Sys time"
    | false, true ->
      Printf.sprintf "; %8s; %8s" "Allocated bytes" "Allocated bytes major"
    | false, false -> ""
  in
  let optional_columns sys_time allocated_bytes allocated_bytes_major =
    match (profile_with_sys_time, profile_with_allocated_bytes) with
    | true, true ->
      Printf.sprintf "; %8.3f; %d; %d" sys_time allocated_bytes
        allocated_bytes_major
    | true, false -> Printf.sprintf "; %8.3f" sys_time
    | false, true ->
      Printf.sprintf "; %d; %d" allocated_bytes allocated_bytes_major
    | false, false -> ""
  in
  if threshold > 0.0 then
    Printf.fprintf oc
      "\n\
       Note: Nodes accounting for less than %2.2f%% of their parent have been \
       ignored.\n\
       %!"
      threshold;
  Printf.fprintf oc "\nAggregated table:\n----------------\n%!";
  let max_name_length =
    List.fold_left
      (fun acc (node : Node.t) -> max acc (String.length node.name))
      0 normal_nodes
  in
  let max_location_length =
    List.fold_left
      (fun acc (node : Node.t) -> max acc (String.length node.location))
      0 normal_nodes
  in
  Printf.fprintf oc "%*s; %*s; %8s; %8s%s\n%!" max_name_length "Name"
    max_location_length "Filename" "Calls" "Time" optional_headers;
  let print_row
    { Node.name
    ; location
    ; calls
    ; time
    ; allocated_bytes
    ; allocated_bytes_major
    ; sys_time
    ; _
    } =
    let time, unit = human time in
    Printf.fprintf oc "%*s; %*s; %8d; %7.2f%1s%s\n%!" max_name_length name
      max_location_length location calls time unit
      (optional_columns sys_time allocated_bytes allocated_bytes_major)
  in
  List.iter print_row normal_nodes;
  if sample_nodes <> [] then begin
    Printf.fprintf oc "\nSamplings\n----------\n%!";
    let stats d =
      let avg =
        Float.Array.fold_left ( +. ) 0.0 d /. float (Float.Array.length d)
      in
      let square x = x *. x in
      let stddev =
        sqrt
          ( Float.Array.fold_left (fun acc x -> acc +. square (x -. avg)) 0.0 d
          /. float (Float.Array.length d) )
      in
      (avg, stddev)
    in
    List.iter
      (fun (node : Node.t) ->
        let avg, stddev = stats node.distrib in
        Printf.fprintf oc "%s: avg = %g, stddev = %g\n%!" (label node) avg
          stddev )
      sample_nodes
  end

let to_json { nodes; label; root } =
  let open Json in
  Map
    [ ( "nodes"
      , ListClosure (Array.length nodes, fun k -> Node.to_json nodes.(k)) )
    ; ("label", String label)
    ; ("root", Int root)
    ]

let output_json oc graph = Json.output oc (to_json graph)
