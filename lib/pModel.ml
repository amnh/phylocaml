(* Types *)

type basic_tcm =
  [ `Const  of int               (* A single cost between differing states. *)
  | `Linear of int * int         (* A cost between states is equal, except indel. *)
  | `TCM    of int array array ] (* General TCM contains a full matrix. *)

type tcm =
  [ `Affine of basic_tcm * int
  | basic_tcm ]

type options =
  { tie_breaker : [ `First | `Last | `Random ];
    alphabetsize : int;
    level : int;
  }

type s =
  { tcm : tcm;
      a : Alphabet.t;
    opt : options;
  }

type t =
  { spec : s;
      cm : int;
  }


(* Functions *)

let of_spec s : t =
  {spec = s; cm = 0;}

let cache : (options * tcm, t) Hashtbl.t = Hashtbl.create 13

let of_spec_cached s : t =
  if Hashtbl.mem cache (s.opt,s.tcm) then
    let t = Hashtbl.find cache (s.opt,s.tcm) in
    {t with spec = s; }
  else
    let result = of_spec s in
    let () = Hashtbl.add cache (s.opt,s.tcm) result in
    result
