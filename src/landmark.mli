(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

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

(** Checks if the profiling is ongoing. *)
val profiling : unit -> bool

(** Where to output results. *)
type profile_output =
  | Silent
    (** disables the automatic output of profiling results when the program
        ends. *)
  | Temporary of string option
    (** writes the results in a temporary files and prints its path on stderr.
    *)
  | Channel of out_channel  (** writes in the results in out_channel. *)

type textual_option = { threshold : float }

(** The output format for the results.*)
type profile_format =
  | JSON  (** Easily parsable export format. *)
  | Textual of textual_option
    (** Console friendly output; nodes below the threshold (0.0 <= threshold <=
        100.0) are not displayed in the callgraph. *)

(** The profiling options control the behavior of the landmark infrastructure.
*)
type profiling_options =
  { debug : bool
      (** Activates a verbose mode that outputs traces on stderr each time the
          landmarks primitives are called. Default: false. *)
  ; allocated_bytes : bool
      (** Also collect {! Gc.allocated_bytes} during profiling. *)
  ; sys_time : bool
      (** Also collect {! Sys.time} timestamps during profiling. *)
  ; recursive : bool
      (** When false, the recursive instances of landmarks (entering a landmark
          that has already been entered) are ignored (the number of calls is
          updated but it does not generate a new node in the callgraph).*)
  ; output : profile_output  (** Specify where to output the results. *)
  ; format : profile_format  (** Specify the output format. *)
  }

(** The default {!profiling_options}. *)
val default_options : profiling_options

(** Sets the options. *)
val set_profiling_options : profiling_options -> unit

(** Get the options. *)
val profiling_options : unit -> profiling_options

(** Starts the profiling. *)
val start_profiling : ?profiling_options:profiling_options -> unit -> unit

(** Stops the profiling. *)
val stop_profiling : unit -> unit

(** Reset the profiling information gathered by the current process. *)
val reset : unit -> unit

(** Export the profiling information of the current process. *)
val export : ?label:string -> unit -> Graph.graph

(** Export the profiling information of the current process; then reset internal
    state. *)
val export_and_reset : ?label:string -> unit -> Graph.graph

(** Aggregate the profiling information (exported by another process) to the
    current one. This should is used by the master process to merge exported
    profiles of workers. *)
val merge : Graph.graph -> unit

(** Save the state of the profiler on a stack to be retrieved later by
    [pop_profiling_state ()]. *)
val push_profiling_state : unit -> unit

(** See [push_profiling_state ()]. *)
val pop_profiling_state : unit -> unit

(** This a redefinition of [Stdlib.raise] to allow generated code to work with
    -no-stdlib.*)
external raise : exn -> 'a = "%raise"
