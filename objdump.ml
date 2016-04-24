open Core_kernel.Std
open Re_perl
open Bap.Std
open Regular.Std
open Cmdliner
open Format
open Option.Monad_infix

include Self()

(* if you are on OSX with macports, this may be:
  /opt/local/bin/x86_64-elf-objdump
*)
let objdump_cmd = "/usr/bin/objdump"
let objdump_opts = "-rd --no-show-raw-insn"
let version = "0.1"


module Symbols = Data.Make(struct
    type t = (string * int64 * int64) list
    let version = version
  end)


(* expected format: [num] <[name]>:
   Note the use of "^\s" to stop greedy globing of the re "+"
*)
let func_start_re = "([0-9A-Fa-f^\\s]+) <(.*)>:"

(* expected format: \s\s[num]:\t[instr]
   Note the use of "^\s" to stop greedy globbing of the re "+"
   Also note this assumes there are no raw instructions
*)
let instr_re = "\\s\\s([0-9A-Fa-f^:]+):\t.*"

let re r =
  Re_pcre.re r |> Re.compile |> Re.execp

let objdump_strip  =
  String.strip ~drop:(fun x -> x = '<' || x = '>' || x = ':' || x = ' ')

let text_to_addr l =
  objdump_strip l |> (^) "0x" |> Int64.of_string

let parse_func_start l =
  if re func_start_re l then
    let xs = String.split_on_chars ~on:[' '] l in
    match xs with
      addr::name::[] -> Some(objdump_strip name, text_to_addr addr)
    | _ -> None
  else
    None

let parse_instr l =
  if re instr_re l then
    let xs = String.split_on_chars ~on:['\t'] l in
    match xs with
      addr::_::[] -> Some(text_to_addr addr)
    | _ -> None
  else
    None

(** Infer an end address

    This is hackish.  We know we have a list of functions that have
    their start address (per objdump).  We set the last address for
    the function to be one less than the following function.

    This leaves the last function (by start address) special: what
    should it's last address be?  We set it to be either the last
    instruction address if known, or the start address of the final
    function if not known.
*)
let rec end_addr funcs omax =
  match funcs with
  | [] -> []
  | (n1,a1) :: ((n2,a2) :: _ as tl) ->
    (n1,a1, Int64.pred a2) :: end_addr tl omax
  | [(n1,a1)] ->
    match omax with None -> [(n1,a1,a1)] | Some(y) -> [(n1,a1,y)]

let print =
 List.iter ~f:(fun (n, a1, a2) -> eprintf "(%08LX %08LX %s)\n" a1 a2 n)


let run_objdump  cmd opts file =
  let anymax a b =
    (* anymax is either tha max, or if one is None, the other *)
    match a,b with
    | Some(_), None -> a
    | None, Some(_) -> b
    | Some(x), Some(y) -> Some(Int64.max x y)
    | None, None -> None
  in
  let rec max_list l =
    match l with
    | [] -> None
    | h :: t -> anymax h (max_list t)
  in
  let process (funcs, omax) l =
    let pf = parse_func_start l in
    let pi = parse_instr l in
    match pf with
      Some(n, a) -> ((n,a)::funcs, max_list [Some(a); pi; omax])
    | None -> (funcs, anymax pi omax)
  in
  let fullcmd = cmd ^ " " ^ opts ^ " " ^ file in
  let ic = Unix.open_process_in fullcmd in
  let (funcs,maddr) = In_channel.fold_lines ~init:([],None) ~f:process ic in
  let sfuncs = List.sort
      ~cmp:(fun (_,a1) (_,a2) -> Int64.compare a1 a2) funcs
  in
  let syms = end_addr sfuncs maddr in
  print syms;
  syms

let register cmd opts =
  let make_id objdump path =
    Data.Cache.digest ~namespace:"objdump" "%s%s"
      (Digest.file path) objdump in
  let syms cmd path =
    let id = make_id cmd path in
    match Symbols.Cache.load id with
    | Some syms -> Some syms
    | None -> match run_objdump cmd opts path with
      | [] -> None
      | syms -> Symbols.Cache.save id syms; Some syms in
  let extract img =
    let addr = Addr.of_int64 in
    Image.filename img >>= syms cmd >>|
    List.map ~f:(fun (n,s,e) -> n,addr s, addr e) >>| Seq.of_list in
  let symbolizer img = extract img >>| Symbolizer.of_blocks in
  Symbolizer.Factory.register Source.Binary name symbolizer


let path : string Term.t =
  let doc = "Specify the path to objdump." in
  Arg.(value & opt file objdump_cmd & info ["path"] ~doc)

let opts : string Term.t =
  let doc = "Specify objdump options. \
             Warning! We rely on *no* raw instructions, i.e., \
             -no-raw-options, during parsing." in
  Arg.(value & opt string objdump_opts & info ["opts"] ~doc)

let info =
  let doc = "Get symbols by calling system objdump." in
  let man = [
    `S "DESCRIPTION";
    `P "This plugin provides a symbolizer based on objdump. \
        Note that we parse objdump output, thus this symbolizer \
        is potentially fragile to changes in objdumps output."
  ] in
  Term.info ~man ~doc "objdump" ~version

let () =
  let run = Term.(const register $path $opts) in
  match Term.eval ~argv ~catch:false (run, info) with
  | `Ok () -> ()
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1

(* uncomment if you want to create a .native app *)
(* let () = *)
(*   let syms = run_objdump  objdump_cmd objdump_opts Sys.argv.(1) in *)
(*   List.iter ~f:(fun (n, a1, a2) -> printf "(%08LX %08LX %s)\n" a1 a2 n) syms *)
