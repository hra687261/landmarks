(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

(** All about exporting profiling results *)

(** A {i callgraph} is a tree of {! node}. *)

(** {3 Callgraph} *)

(** The type of callgraphs. *)
type t =
  { nodes : Cg_node.t array
  ; label : string
  ; root : Cg_node.id
  }

(** Returns all nodes of a graph. *)
val nodes : t -> Cg_node.t list

(** Returns the root of a graph. *)
val root : t -> Cg_node.t

(** Returns the children of node. *)
val children : t -> Cg_node.t -> Cg_node.t list

(** Change the root of a graph *)
val subgraph : t -> Cg_node.t -> t

(** Returns a fully qualified name if it is needed. *)
val label : t -> Cg_node.t -> string

(** Build a graph from a list of nodes. *)
val of_nodes : ?label:string -> ?root:Cg_node.id -> Cg_node.t list -> t

(** {3 Traversal} *)

(** [path_dfs f g graph] traverses the graph in the depth-first fashion starting
    from the root. At each step we call [f visited path v] or [g path v] where
    [v] is the visited node and [path] is the path from the root that led us to
    that node. The function [g] is called when the visited node [v] belongs to
    [path]; it indicates a loop (and the traversal does not continue with the
    children of g). The function [f] is called when [v] does not belong to
    [path]. The flag [visited] is true when the vertex has already been visited.
*)
val path_dfs :
     (bool -> Cg_node.t list -> Cg_node.t -> unit)
  -> (Cg_node.t list -> Cg_node.t -> unit)
  -> t
  -> unit

(** A specialization of [path_dfs] that does not need to read the visited flag.
    The returned values of the first function tells whether or not the traversal
    should continue visiting the children of the current node. *)
val dfs :
     (Cg_node.t list -> Cg_node.t -> bool)
  -> (Cg_node.t list -> Cg_node.t -> unit)
  -> t
  -> unit

(** {3 Utility functions} *)

(** Returns the depth to the root of the node (it is better to partially apply
    the function, if you need to call multiple times on the same graph). *)
val depth : t -> Cg_node.t -> int

(** Returns the oldest ancestor of a node that is not the root (if it exists) or
    the root if it does not exist. *)
val shallow_ancestor : t -> Cg_node.t -> Cg_node.t

(** Returns an arbitrary number between 0.0 and 1.0. *)
val intensity : ?proj:(Cg_node.t -> float) -> t -> Cg_node.t -> float

(** Compute the sum of all calls field. *)
val total_number_of_calls : t -> int

(** {3 Simplification / Merge / Quotienting.} *)

(** Remove the unaccessible nodes from a graph. *)
val prune : t -> t

(** [aggregate_landmarks g] computes the quotient by the relation "being an
    instance of the same landmark". *)
val aggregate_landmarks : t -> t

(** {3 Output} *)

(** Pretty printed output of a call graph on an out_channel. *)
val output : ?threshold:float -> out_channel -> t -> unit

(** Output a JSON representation of a call graph on an out_channel. *)
val output_json : out_channel -> t -> unit
