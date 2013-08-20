type t =
  { codes : int array array;
    weights : float array;
    states : int array;
    cost : float; }

type spec = t

let filter_codes _ _ = failwith "TODO"
let filter_codes_comp _ _ = failwith "TODO"
let cardinal _ = failwith "TODO"
let get_codes _ = failwith "TODO"
let mem _ _ = failwith "TODO"
let union _ _ _ = failwith "TODO"
let compare _ _ = failwith "TODO"
let recode _ _ = failwith "TODO"

let median_1 _ x = x
let median_2 _ x y =
  let test_one x_i y_i =
    let m_i = x_i land y_i in
    if 0 = m_i
      then (x_i lor y_i),1
      else m_i,0
  in
  let cost = ref 0.0 in
  let m =
    Array.mapi
      (fun i _ ->
        let r,c = test_one x.states.(i) y.states.(i) in
        cost := !cost +. (x.weights.(i) *. (float_of_int c));
        r)
      x.states
  in
  { x with states = m; cost = !cost; }
  
let median_n _ _ _ = failwith "TODO"

let adjust_3 _ _ _ _ = failwith "TODO"

let adjust_n _ _ _ _ = failwith "TODO"

let cost _ = failwith "TODO"
let root_cost _ = failwith "TODO"

let distance_1 _ _ _ _ = failwith "TODO"
let distance_2 _ _ _ _ _ _ = failwith "TODO"

let to_string _ = failwith "TODO"

(** For Parsing (not apart of NodeData) *)

let of_string _ = failwith "TODO"
let of_parser _ = failwith "TODO"

let create_spec _ _ = failwith "TODO"
