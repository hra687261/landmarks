(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

type t =
  | String of string
  | Int of int
  | Float of float
  | Map of (string * t) list
  | List of t list
  | ListClosure of int * (int -> t)

(** Output JSON on an out_channel. *)
val output : out_channel -> t -> unit
