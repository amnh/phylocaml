open Phylocaml_pervasives

type spec = unit

type s =
    | Sequence   of SequenceData.t
    | Likelihood of LikelihoodData.t

type t = {
    data : s IntMap.t;
}

let filter_codes _ _ = failwith "TODO"
let filter_codes_comp _ _ = failwith "TODO"
let cardinal _ = failwith "TODO"
let get_codes _ = failwith "TODO"
let mem _ _ = failwith "TODO"
let union _ _ _ = failwith "TODO"
let compare _ _ = failwith "TODO"

let median_1 _ _ = failwith "TODO"
let median_2 _ _ _ = failwith "TODO"
let median_n _ _ _ = failwith "TODO"

let adjust_3 _ _ _ _ = failwith "TODO"
let adjust_n _ _ _ = failwith "TODO"

let recode _ _ = failwith "TODO"

let cost _ = failwith "TODO"
let root_cost _ = failwith "TODO"
let leaf_cost _ = failwith "TODO"

let distance_1 _ _ = failwith "TODO"
let distance_2 _ _ _ = failwith "TODO"

let to_string _ = failwith "TODO"


(** For Parsing (not apart of NodeData) *)
let empty =
    {data = IntMap.empty; }

let add_t s t =
    let n = IntMap.cardinal t.data in
    {data = IntMap.add n s t.data;}

let add_likelihood_data x t =
    add_t (Likelihood (LikelihoodData.of_parser x)) t

let add_sequence_data x t =
    add_t (Sequence (SequenceData.of_parser x)) t
