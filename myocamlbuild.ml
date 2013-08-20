open Ocamlbuild_plugin
open Command

let rec arg_weave p = function
  | x::xs -> (A p) :: (A x) :: arg_weave p xs
  | []    -> []

let cc      = "gcc"
let cflags  = arg_weave "-ccopt" ["-O2"; "-Wall"; "-pedantic";"-Wextra"]
let clibs   = ""
let mlflags = ["-w"; "@a"]

let headers =
   let bv_ w = "lib/bv"^w^".h" in
   let bv = [bv_ "8"; bv_ "16"; bv_ "32"; bv_ "64"] in
  "lib/seq.h" :: "lib/mlmodel.h" :: "lib/phyloc.h" :: bv

let () = dispatch begin function
  | After_rules ->
    let bitvector_rule prefix filename extension oth_ext width options : unit =
      let deps = match oth_ext with
        | Some x -> ["lib/"^prefix^width^filename^x]
        | None   -> []
      in
      let dep  = "lib/"^prefix^filename^extension
      and prod = "lib/"^prefix^width^filename^extension in
      rule prod
        ~prod ~deps:(dep::deps)
        begin fun evn _build ->
          Cmd (S [A "sed"; A"-e"; A("s/bv\\.h/bv"^width^".h/g");
                           A"-e"; A("s/bv_/bv"^width^"_/g");
                           Sh ("< "^dep); Sh("> "^prod)])
        end
    in
    let bv_widths = ["8";"16";"32";"64"] in
    List.iter (fun w -> bitvector_rule "bv" "" ".c" (Some ".h") w []) bv_widths;
    List.iter (fun w -> bitvector_rule "bv" "" ".h" None w []) bv_widths;
    (* List.iter (fun w -> bitvector_rule "bv" "_sse" ".c" w [`SSE]) bv_widths; *)
    (* List.iter (fun w -> bitvector_rule "bv" "_sse" ".h" w [`SSE]) bv_widths; *)

    flag ["c"; "use_bv8"]  (S [A"-ccopt";A"-DWIDTH=8" ]);
    flag ["c"; "use_bv16"] (S [A"-ccopt";A"-DWIDTH=16"]);
    flag ["c"; "use_bv32"] (S [A"-ccopt";A"-DWIDTH=32"]);
    flag ["c"; "use_bv64"] (S [A"-ccopt";A"-DWIDTH=64"]);

    dep ["ocaml";"link"; "use_phyloc"] ["libphyloc.a"];
    dep ["c"; "compile"]  headers;

    flag ["ocaml"; "link";"use_phyloc"]   (S [A"-cclib";A"-lphyloc"]);
    flag ["ocaml"; "compile"] (S [A"-cc";A cc]);
    flag ["ocaml"; "compile"] (S cflags);
    ()
  | _ -> ()
end
