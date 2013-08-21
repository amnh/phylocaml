open Ocamlbuild_plugin
open Command

(** Modifiable parameters *)
let cc      = "gcc"
let cflags  = ["-O2"; "-Wall"; "-pedantic";"-Wextra"]
let clibs   = ""
let mlflags = ["-w"; "@a"]
let vectorization = None

(** Constants *)
let bv_width = ["8";"16";"32";"64"]

(** Helper functions *)
let rec arg_weave p = function
  | x::xs -> (A p) :: (A x) :: arg_weave p xs
  | []    -> []

let ($) a b = a b
let (|>) a b = b a

(** C-Header files *)
let headers =
  let headers = 
    ["lib/seq.h"; "lib/mlmodel.h"; "lib/phyloc.h"]
  in
  let bv_ w =
    let base = "lib/bitvector/bv"^w^".h" in 
    match vectorization with
    | None          -> [base]
    | Some `SSE4_2
    | Some `SSE4_1
    | Some `SSE3_0
    | Some `SSE2_0  -> ["lib/bitvector/bv"^w^"_sse.h"; base]
    | Some `AVX     -> ["lib/bitvector/bv"^w^"_avx.h"; base]
    | Some `Altivec -> ["lib/bitvector/bv"^w^"_alti.h"; base]
    | Some `Neon    -> ["lib/bitvector/bv"^w^"_neon.h"; base]
  in
  List.rev_append headers (List.flatten $ List.map bv_ bv_width)

let () = dispatch begin function
  | After_rules ->
    let bitvector_rule filename extension dep_ext width : unit =
      let dep  = "lib/bitvector/bv"^filename^extension
      and prod = "lib/bitvector/bv"^width^filename^extension in
      let deps = match dep_ext with
        | Some x -> [dep;"lib/bitvector/bv"^width^filename^x]
        | None   -> [dep]
      in
      rule ("Generate "^prod) ~prod ~deps
        begin fun _ _ ->
          Cmd (S [A "sed"; A"-e"; A("s/bv\\.h/bv"^width^".h/g");
                           A"-e"; A("s/bv_/bv"^width^"_/g");
                           Sh ("< "^dep); Sh("> "^prod)])
        end
    in
    List.iter
      (fun namext ->
        List.iter (bitvector_rule namext ".c" (Some ".h")) bv_width;
        List.iter (bitvector_rule namext ".h" (None)     ) bv_width)
      [""; "_sse"; "_neon"; "_avx"; "_alti"];

    let bv_cflags width =
      let cflags = ["-DWIDTH="^width] in
      let cflags = match vectorization with
        | None          -> cflags
        | Some `SSE4_2  -> "-msse4.2"::cflags
        | Some `SSE4_1  -> "-msse4.1"::cflags
        | Some `SSE3_0  -> "-msse3"::cflags
        | Some `SSE2_0  -> "-msse2"::cflags
        | Some `AVX     -> "-mavx"::cflags
        | Some `Altivec -> "-maltivec"::cflags
        | Some `Neon    -> "-funsafe-math-optimizations"::"-mfpu=‘neon’"::cflags
      in
      arg_weave "-ccopt" cflags
    in
    List.iter (fun w -> flag ["c"; "use_bv"^w] (S (bv_cflags w))) bv_width;

    dep ["ocaml";"link"; "use_phyloc"] ["libphyloc.a"];
    dep ["c"; "compile"]  headers;

    flag ["ocaml"; "link";"use_phyloc"]   (S [A"-cclib";A"-lphyloc"]);
    flag ["ocaml"; "compile"] (S [A"-cc";A cc]);
    flag ["ocaml"; "compile"] (S (arg_weave "-ccopt" cflags));
    ()
  | _ -> ()
end
