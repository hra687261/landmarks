(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

module Node = Node
module Graph = Graph

(** The main module *)

(** This function is used by the landmark infrastructure to measure the number
    of cycles inside landmarks. *)
external clock : unit -> (Int64.t[@unboxed])
  = "caml_highres_clock" "caml_highres_clock_native"
[@@noalloc]

exception LandmarkFailure of string

(** {3 Landmarks} *)

(** {i Landmarks} identify portions of code, they are registered with the
    function {! register} and delimited by {! enter} and {! exit}. *)

(** The type of landmarks. *)
type landmark

(** [register name] registers a new landmark. Note that landmarks are identified
    by location + name (or by [id] if provided). If you register a landmark
    twice the second call returns a physically equal value to the first call (if
    you provide [id] the name & location of the second call is ignored). *)
val register : ?id:string -> ?location:string -> string -> landmark

(** [landmark_of_id id] return the landmark currently identify by [id]. *)
val landmark_of_id : string -> landmark option

(** Begins a landmark block. /!\ Landmark blocks should be well-nested,
    otherwise a failure will be raised during profiling. *)
val enter : landmark -> unit

(** Ends a landmark block. *)
val exit : landmark -> unit

(** Puts landmark blocks around a function (and close the block and re-raise in
    case of uncaught exception). *)
val wrap : landmark -> ('a -> 'b) -> 'a -> 'b

(** Puts landmark blocks around a function without catching exceptions. *)
val unsafe_wrap : landmark -> ('a -> 'b) -> 'a -> 'b

(** {3 Counter and samplers} *)

(** {i Counters} are similar to landmarks except they represent empty pieces of
    code. Their only primitive is {!increment} which adds a constant to the
    field [calls]. {i Samplers} are used to collect floats. *)

(** The type of counters. *)
type counter

(** [register_counter name] registers a new counter. Should always be called at
    top-level. *)
val register_counter : string -> counter

(** Increments the number of calls attached to the counter. *)
val increment : ?times:int -> counter -> unit

(** The type of samplers. *)
type sampler

(** [register_counter name] registers a new sampler. *)
val register_sampler : string -> sampler

(** Collects a float. *)
val sample : sampler -> float -> unit

(** {3 Manage profiling} *)

module Options = Options

(** Starts the profiling. *)
val start_profiling : ?profiling_options:Options.t -> unit -> unit

(** Stops the profiling. *)
val stop_profiling : unit -> unit

(** Reset the profiling information gathered by the current process. *)
val reset : unit -> unit

(** Export the profiling information of the current process. *)
val export : ?label:string -> unit -> Graph.t

(** Export the profiling information of the current process; then reset internal
    state. *)
val export_and_reset : ?label:string -> unit -> Graph.t

(** Aggregate the profiling information (exported by another process) to the
    current one. This should is used by the master process to merge exported
    profiles of workers. *)
val merge : Graph.t -> unit

(** Save the state of the profiler on a stack to be retrieved later by
    [pop_profiling_state ()]. *)
val push_profiling_state : unit -> unit

(** See [push_profiling_state ()]. *)
val pop_profiling_state : unit -> unit

(** This a redefinition of [Stdlib.raise] to allow generated code to work with
    -no-stdlib.*)
external raise : exn -> 'a = "%raise"
