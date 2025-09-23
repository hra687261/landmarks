(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

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
type t =
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
val default : t

val parse_env : string -> t

(** Checks if the profiling is ongoing. *)
val ongoing
  (* TODO: move this somewhere else, it is not really an option... *) :
  unit -> bool

val set_ongoing : bool -> unit

(** Get the options. *)
val get_current : unit -> t

(** Sets the options. *)
val set_current : t -> unit

val with_debug : unit -> bool

val with_allocated_bytes : unit -> bool

val with_sys_time : unit -> bool

val recursive : unit -> bool

val output : unit -> profile_output

val format : unit -> profile_format
