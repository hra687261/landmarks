(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

(** A {! node} is an instance of a {!Landmark.landmark} representing the
    entering and exiting of instrumented code in the execution path. *)

(** Identifies nodes *)
type id = int

(** The kind of node. *)
type kind =
  | Normal  (** Usual landmarks *)
  | Root  (** The special node that started with the profiling. *)
  | Counter  (** Counters (see {!Landmark.counter}) *)
  | Sampler  (** Samplers (set {!Landmark.sampler}) *)

(** The type exported view of a node. *)
type t =
  { id : id  (** Unique identifier. *)
  ; kind : kind
  ; landmark_id : string  (** The node is an instance of this landmark. *)
  ; name : string  (** Name of the landmark (see {! Landmark.register}). *)
  ; location : string
      (** Location of the landmark (see {! Landmark.register}). *)
  ; calls : int  (** Number of time this node was entered. *)
  ; time : float  (** Time (in cycles) spent between enter and exit. *)
  ; children : id list
      (** The list of instances of landmarks that was entered while the node was
          opened. *)
  ; sys_time : float  (** Time (using Sys.time) spent between enter and exit. *)
  ; allocated_bytes : int  (** Allocated bytes between enter and exit. *)
  ; allocated_bytes_major : int
      (** Allocated bytes in the major heap between enter and exit. *)
  ; distrib : floatarray
      (** For samplers only. The list of collected samples. *)
  }

val compare : t -> t -> int

val compare_time : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val is_normal_or_root : t -> bool

val is_sampler : t -> bool

val to_json : t -> Json.t
