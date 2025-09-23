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

open Format

let rec output oc = function
  | String s -> fprintf oc "\"%s\"" (String.escaped s)
  | Int n -> fprintf oc "%d" n
  | Float f -> fprintf oc "%f" f
  | Map l ->
    fprintf oc "{@,";
    let first = ref true in
    List.iter
      (fun (name, json) ->
        if !first then first := false else fprintf oc ",@,";
        fprintf oc "@[<v 2>%S: %a@]" name output json )
      l;
    fprintf oc "@;<0 -2>}"
  | List [] -> fprintf oc "[]"
  | List [ x ] -> fprintf oc "[%a]" output x
  | List l when List.for_all (function Int _ -> true | _ -> false) l ->
    fprintf oc "[%s]"
      (String.concat ", "
         (List.map (function Int x -> string_of_int x | _ -> assert false) l) )
  | List l ->
    fprintf oc "[@,";
    let first = ref true in
    List.iter
      (fun json ->
        if !first then first := false else fprintf oc ",@,";
        fprintf oc "@[<v 2>%a@]" output json )
      l;
    fprintf oc "@;<0 -2>]"
  | ListClosure (n, f) ->
    fprintf oc "[@,";
    for k = 0 to n - 1 do
      let json = f k in
      if k > 0 then fprintf oc ",@,";
      fprintf oc "@[<v 2>%a@]" output json
    done;
    fprintf oc "@;<0 -2>]"

let output oc = fprintf (formatter_of_out_channel oc) "@[<v 2>%a@]@." output
