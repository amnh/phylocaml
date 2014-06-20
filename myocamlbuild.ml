open Ocamlbuild_plugin
open Command

(** Modifiable parameters *)
let cc      = "gcc"
let cflags  = ["-O2"; "-Wall"; "-pedantic";"-Wextra"]
let clibs   = ["-llapack";"-lblas";"-lgfortran"]
let static  = true
let mlflags = ["-w"; "@a";"-warn-error";"-a"]
let vectorization = None

let major,minor =
  let rec get_until i str acc =
    if (i >= (String.length str)) || ('.' = String.get str i)
      then String.concat "" (List.rev_map (String.make 1) acc),(i+1)
      else
          get_until (i+1) str ((String.get str i)::acc)
  in
  let major,n = get_until 0 Sys.ocaml_version [] in
  let minor,_ = get_until n Sys.ocaml_version [] in
  int_of_string major, int_of_string minor

(** Constants *)
let bv_width = ["8";"16";"32";"64"]

(** Helper functions *)
let rec arg_weave p = function
  | x::xs -> (A p) :: (A x) :: arg_weave p xs
  | []    -> []

let arg x = A x
 
(** C-Header files *)
let headers =
  let headers = ["lib/seq.h"; "lib/mlmodel.h"; "lib/phyloc.h"] in
  let bv_ w =
    let base = "lib/bitvector/bv"^w^".h" in
    match vectorization with
    | None          -> [base]
    | Some `SSE4a
    | Some `SSE4_2
    | Some `SSE4_1
    | Some `SSE4
    | Some `SSSE3_0
    | Some `SSE3_0
    | Some `SSE2_0  -> ["lib/bitvector/bv"^w^"_sse.h"; base]
    | Some `AVX     -> ["lib/bitvector/bv"^w^"_avx.h"; base]
    | Some `Altivec -> ["lib/bitvector/bv"^w^"_alti.h"; base]
    | Some `Neon    -> ["lib/bitvector/bv"^w^"_neon.h"; base]
    | Some `Auto    -> failwith "CURRENTLY CANNOT AUTODETECT VECTOR LIBRARY"
  in
  List.rev_append headers (List.flatten (List.map bv_ bv_width))

let () = dispatch begin function
  | Before_options ->
    let ocamlfind x = S[A"ocamlfind";arg x] in
    Options.ocamlmktop := ocamlfind "ocamlmktop"; (* fixed in >=4.1.0 *)
    ()
  | After_rules ->
    (* generate all rules for scripted C and header files *)
    let bitvector_rule vectype extension dep_extension width =
      let dep  = "lib/bitvector/bv"^vectype^extension
      and prod = "lib/bitvector/bv"^width^vectype^extension in
      let deps = match dep_extension with
        | Some d_ext -> [dep;"lib/bitvector/bv"^width^vectype^d_ext]
        | None       -> [dep]
      in
      rule ("Generate "^prod) ~prod ~deps
        begin fun _ _ ->
          Cmd (S [A "sed"; A"-e"; A("s/bv\\.h/bv"^width^".h/g"); (* replace refrences to bv.h -> bvW.h *)
                           A"-e"; A("s/bv_/bv"^width^"_/g");     (* replace bv_* with bvW_* *)
                           Sh ("< "^dep); Sh("> "^prod)])        (* pass in dep, pipe to prod *)
        end
    in
    List.iter
      (fun namext ->
        List.iter (bitvector_rule namext ".c" (Some ".h")) bv_width;
        List.iter (bitvector_rule namext ".h" (None)     ) bv_width)
      [""; "_sse"; "_neon"; "_avx"; "_alti"];

    (* generate flags for vectorization compilation *)
    let bv_cflags width =
      let cflags = ["-DWIDTH="^width] in
      let cflags = match vectorization with
        | None          -> cflags
        | Some `Auto    -> "-march=native"::cflags
        | Some `SSE4a   -> "-msse4a"::cflags
        | Some `SSE4_2  -> "-msse4.2"::cflags
        | Some `SSE4_1  -> "-msse4.1"::cflags
        | Some `SSE4    -> "-msse4"::cflags
        | Some `SSSE3_0 -> "-mssse3"::cflags
        | Some `SSE3_0  -> "-msse3"::cflags
        | Some `SSE2_0  -> "-msse2"::cflags
        | Some `AVX     -> "-mavx"::cflags
        | Some `Altivec -> "-maltivec"::cflags
        | Some `Neon    -> "-funsafe-math-optimizations"::"-mfpu=‘neon’"::cflags
      in
      arg_weave "-ccopt" cflags
    in
    List.iter (fun w -> flag ["c";"use_bv"^w] (S (bv_cflags w))) bv_width;

    (* declare use_phylocaml and include_phylocaml for the tests, apps, bench *)
    (* ocaml_lib "phylocaml"; *)

    (* pre-process compatibility module *)
    let compatibility_options =
      if major < 4 || ((major = 4) && minor <= 0)
        then [A"-pp";A"camlp4of -DCOMPATIBILITY"]
        else [A"-pp";A"camlp4of -UCOMPATIBILITY"]
    in
    flag ["ocaml";"use_compatibility";"doc"]      (S compatibility_options);
    flag ["ocaml";"use_compatibility";"ocamldep"] (S compatibility_options);
    flag ["ocaml";"use_compatibility";"compile"]  (S compatibility_options);
    flag ["ocaml";"use_compatibility";"mktop"]    (S compatibility_options);

    (* testing pre-process flags *)
    let testing_options = [A"-pp";A"camlp4of -UUSE_EXTERNAL_LINKING"] in
    flag ["ocaml";"test";"compile" ] (S testing_options);
    flag ["ocaml";"test";"ocamldep" ] (S testing_options);

    (* dependencies for c-stubs *)
    dep ["c"; "compile"] headers;
    (* dep  ["link"; "ocaml"; "use_phyloc"] ["libphyloc.a"]; *)

    (* flags for c compilation/linking *)
    flag ["ocaml"; "compile"] (S [A"-cc";A cc]);
    flag ["c"; "compile"]     (S (arg_weave "-ccopt" cflags));
    flag ["c"; "link"]        (S (arg_weave "-cclib" clibs));
    flag ["ocamlmklib"; "c"]  (S (arg_weave "-cclib" clibs));

    (* compile ocaml w/ c-stubs *)
    flag ["link";"ocaml";"use_phyloc";"byte"]
      (S[A"-dllib";A"-lphyloc";A"-cclib";A"-lphyloc";A"-cclib";A"-L." ]);
    flag ["link";"ocaml";"use_phyloc";"native"]
      (S[A"-cclib";A"-lphyloc";A"-cclib";A"-L."]);

    (* flags for ocaml compiling/linking *)
    flag ["ocaml"; "compile"]     (S (List.map arg mlflags));
    flag ["ocaml"; "link"]        (S (arg_weave "-cclib" clibs));
    flag ["ocaml";"link";"byte"]  (arg "-custom");
    ()
  | _ -> ()
end
