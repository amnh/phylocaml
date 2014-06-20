open OUnit2
open TestInternal

(* A list of branch length to compose a model for *)
let bls = [ min_float; ~-.1.0;  0.0;  1.0; 100.0; max_float ]
(* A list if the above bls should compose to a matrix *)
and bfs = [      true; false; true; true;  true;     false ]

let all_spec : MlModel.spec list =
  let rec loop_ f acc = match f () with
    | None -> acc
    | Some x -> loop_ f (x::acc)
  in
  loop_ (MlModel.enum_models None Alphabet.dna) []

let tests =
  "MlModel Tests" >:::
    [
      "Enumerate Models" >::
       (fun _ctxt ->
          let () = ignore (List.map (MlModel.create) all_spec) in
          ());

      "Compose Models w/ branches" >::
       (fun _ctxt ->
          let verify composed = true in
          let single model (truth,branch) =
            try MlModel.compose model branch |> verify
            with _ -> not truth
          in
          List.iter
            (fun s ->
              let m = MlModel.create s in
              List.iter
                (fun x ->
                  let msg = "Failed Verifying "^(MlModel.short_name m) in
                  assert_bool msg (single m x))
                (List.combine bfs bls))
            all_spec);

      "Number of Parameters" >::
       (fun _ctxt -> ());

      "Gamma Rate Properties" >::
       (fun _ctxt ->
          let verify_rates cats rates =
            let p = 1.0 /. cats in
            let rsps = ref 0.0 in
            for i = 0 to (Bigarray.Array1.dim rates) - 1 do
              rsps := !rsps +. (p *. rates.{i});
            done;
            !rsps =. 1.0
          in
          List.iter
            (fun c ->
              let r = Random.float 50.0 in
              let rates = MlModel.gamma_rates r r c in
              let msg = Printf.sprintf "Gamma Rate = %d / %f failed" c r in
              assert_bool msg @@ verify_rates (float_of_int c) rates)
            (1 -- 10));
    ]

