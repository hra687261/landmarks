(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright (C) 2000-2025 LexiFi                                    *)

type profile_output =
  | Silent
  | Temporary of string option
  | Channel of out_channel

type textual_option = { threshold : float }

type profile_format =
  | JSON
  | Textual of textual_option

type t =
  { debug : bool
  ; allocated_bytes : bool
  ; sys_time : bool
  ; recursive : bool
  ; output : profile_output
  ; format : profile_format
  }

let default =
  { debug = false
  ; allocated_bytes = true
  ; sys_time = false
  ; recursive = false
  ; output = Channel stderr
  ; format = Textual { threshold = 1.0 }
  }

let split c s =
  let open String in
  let res = ref [] in
  let pos = ref 0 in
  let len = length s in
  while
    match index_from_opt s !pos c with
    | None ->
      res := sub s !pos (len - !pos) :: !res;
      false
    | Some k ->
      res := sub s !pos (k - !pos) :: !res;
      pos := k + 1;
      !pos < len
      ||
      ( res := "" :: !res;
        false )
  do
    ()
  done;
  List.rev !res

let starts_with ~prefix x =
  let n = String.length prefix in
  String.length x >= n && String.equal prefix (String.sub x 0 n)

let parse_env s =
  let open Printf in
  let debug = ref false in
  let format = ref (Textual { threshold = 1.0 }) in
  let output = ref (Channel stderr) in
  let sys_time = ref false in
  let recursive = ref false in
  let allocated_bytes = ref false in
  let split_trim c s = List.map String.trim (split c s) in
  let warning s = eprintf "[LANDMARKS] %s.\n%!" s in
  let parse_option s =
    let invalid_for opt given =
      warning
        (sprintf "The argument '%s' in not valid for the option '%s'" given opt)
    in
    let expect_no_argument opt =
      warning (sprintf "The option '%s' expects no argument" opt)
    in
    match split_trim '=' s with
    | [] -> ()
    | [ "debug" ] -> debug := true
    | "debug" :: _ -> expect_no_argument "debug"
    | [ "threshold"; percent ] -> begin
      match !format with
      | Textual _ ->
        let threshold = try Some (float_of_string percent) with _ -> None in
        begin
          match threshold with
          | None ->
            warning (Printf.sprintf "Unable to parse threshold '%s'" percent)
          | Some threshold -> format := Textual { threshold }
        end
      | _ ->
        warning
          (Printf.sprintf
             "The option threshold only makes sense with the 'textual' format." )
    end
    | [ "format"; "textual" ] -> begin
      match !format with
      | Textual _ -> ()
      | _ -> format := Textual { threshold = 1.0 }
    end
    | [ "format"; "json" ] -> format := JSON
    | [ "format"; unknown ] -> invalid_for "format" unknown
    | [ "output"; "stderr" ] -> output := Channel stderr
    | [ "output"; "stdout" ] -> output := Channel stdout
    | [ "output"; temporary ] when starts_with ~prefix:"temporary" temporary ->
      begin
      match split_trim ':' temporary with
      | [ "temporary" ] -> output := Temporary None
      | [ "temporary"; dir_spec ] -> begin
        match split_trim '"' dir_spec with
        | [ ""; dir; "" ] -> output := Temporary (Some dir)
        | [ dir ] -> output := Temporary (Some dir)
        | _ -> invalid_for "output" temporary
      end
      | _ -> invalid_for "output" temporary
    end
    | [ "output"; file_spec ] -> (
      match split_trim '"' file_spec with
      | [ ""; file; "" ] | [ file ] -> (
        try output := Channel (open_out file)
        with _ -> warning (sprintf "Unable to open '%s'" file) )
      | _ -> invalid_for "output" file_spec )
    | [ "time" ] -> sys_time := true
    | "time" :: _ -> expect_no_argument "time"
    | [ "recursive" ] -> recursive := true
    | "recursive" :: _ -> expect_no_argument "recursive"
    | [ "allocation" ] -> allocated_bytes := true
    | "allocation" :: _ -> expect_no_argument "allocation"
    | [ "off" ] -> raise Exit
    | "off" :: _ -> expect_no_argument "off"
    | [ "auto" ] | [ "remove" ] | [ "threads" ] ->
      () (* read by the ppx extension *)
    | "auto" :: _ -> expect_no_argument "auto"
    | "remove" :: _ -> expect_no_argument "remove"
    | "threads" :: _ -> expect_no_argument "threads"
    | [ "" ] | [ "on" ] | [ "1" ] -> ()
    | opt :: _ :: _ -> warning (Printf.sprintf "To many '=' after '%s'" opt)
    | unknown :: _ -> warning (sprintf "Unknown option '%s'" unknown)
  in
  List.iter parse_option (split_trim ',' s);
  { debug = !debug
  ; allocated_bytes = !allocated_bytes
  ; sys_time = !sys_time
  ; output = !output
  ; format = !format
  ; recursive = !recursive
  }

let ongoing_ref = ref false

let with_debug_ref = ref false

let with_allocated_bytes_ref = ref false

let with_sys_time_ref = ref false

let output_ref = ref Silent

let format_ref = ref (Textual { threshold = 1.0 })

let recursive_ref = ref false

let ongoing () = !ongoing_ref

let set_ongoing ongoing = ongoing_ref := ongoing

let with_debug () = !with_debug_ref

let with_allocated_bytes () = !with_allocated_bytes_ref

let with_sys_time () = !with_sys_time_ref

let recursive () = !recursive_ref

let output () = !output_ref

let format () = !format_ref

let set_current { debug; allocated_bytes; sys_time; output; format; recursive }
    =
  with_allocated_bytes_ref := allocated_bytes;
  with_sys_time_ref := sys_time;
  with_debug_ref := debug;
  output_ref := output;
  format_ref := format;
  recursive_ref := recursive

let get_current () =
  { debug = !with_debug_ref
  ; allocated_bytes = !with_allocated_bytes_ref
  ; sys_time = !with_sys_time_ref
  ; recursive = !recursive_ref
  ; output = !output_ref
  ; format = !format_ref
  }
