open Ocamlbuild_plugin
open Command

let static  = true
let dumprtl = false
let unsafe  = false
let profile = false

let ocamlfind_query pkg =
  let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
  Ocamlbuild_pack.My_unix.run_and_open cmd (fun ic -> input_line ic)

let split string =
  let len = String.length string in
  let res = ref [] in
  let cur = Buffer.create 29 in
  for i = 0 to len - 1 do
    if string.[i] = ' ' then begin
      res := (Buffer.contents cur) :: !res;
      Buffer.clear cur;
    end else begin
      Buffer.add_char cur string.[i];
    end;
  done;
  res := (Buffer.contents cur) :: !res;
  List.rev !res

let headers =
    [ "seq.h"; ]

let dispatch_fn = function
  | After_rules ->

    (* Adds [c] object dependencies to _tags file by adding 'linkdep(foo.o)'.
     * This can extend to libraries with 'linkdep(libfoo.a)' based on clib. *)
    pdep ["link"] "linkdep" (fun param -> split param);

    dep ["compile"; "c"] headers;

    if static then
      flag ["link"; "ocaml"; "byte"] (A"-custom");

    (* Additional Optional Compilation Options **)
    if unsafe then begin
      flag ["ocaml"; "compile"] (S [A"-unsafe"]);
      flag ["ocaml"; "compile"] (S [A"-noassert"]);
      flag ["ocaml"; "compile"; "native"]  (S [A"-inline";A"1000"]);
      flag ["ocaml"; "compile"] (S [A"-ccopt";A"-DNDEBUG"]);
      flag ["ocaml"; "compile"] (S [A"-ccopt";A"-finline-limit=1000"]);
      flag [    "c"; "compile"] (S [A"-ccopt";A"-DNDEBUG"]);
      flag [    "c"; "compile"] (S [A"-ccopt";A"-finline-limit=1000"]);
    end;
    if dumprtl then begin
      flag ["c"; "compile"] (S [A"-ccopt";A"-fdump-rtl-expand"]);
      flag ["c"; "compile"] (S [A"-ccopt";A"-fno-inline-functions"]);
      flag ["c"; "compile"] (S [A"-ccopt";A"-finline-limit=0"]);
      flag ["c"; "link"]    (S [A"-ccopt";A"-fdump-rtl-expand"]);
      flag ["c"; "link"]    (S [A"-ccopt";A"-fno-inline-functions"]);
      flag ["c"; "link"]    (S [A"-ccopt";A"-finline-limit=0"]);
    end;
    if profile then begin
      flag["ocaml"; "native"; "link"] (S [A"-p"]);
      flag["ocaml"; "native"; "compile"] (S [A"-p"]);
      flag["c";"compile"] (S [A"-ccopt";A"-pg"]);
      flag["c";"link"] (S [A"-cclib";A"-pg"]);
    end;
    ()
  | _ -> ()

let () = dispatch dispatch_fn
