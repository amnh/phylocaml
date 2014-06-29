open Internal

type t = Tree.t

type jxn_state =
  {  join_locations : EdgeSet.t
   reroot_locations : EdgeSet.t;
     break_topology : Tree.t;
     break_delta    : break Topology.delta;
  }

type full_state = 
  { break_locations : EdgeSet.t;
           topology : Tree.t;
          jxn_state : jxn_state; }

type break_locations : Tree.t -> EdgeSet.t

type jxn_locations : Tree.t -> Topology.handle -> JxnSet.t

type choose_edge : EdgeSet.t -> edge

type choose_jxn : JxnSet.t -> Topology.jxn



let generate_tbr ?(choose_break=EdgeSet.choose) ?(choose_join=EdgeSet.choose) ?break_edges ?join_jxn ?reroot_jxn : (module Neighborhood.S) =
  module TBR =
    struct
      type t = Tree.t
      type d = full_state

      let generate_join_locations tree break_delta =
        failwith "TODO"

      let break_process_tree t d =
        if EdgeSet.is_empty break_locations
          then None, d
          else begin
            let e = choose_break d.break_locations in
            let bt,bd = Tree.break e t in
            let js,rs = generate_jxns_locations bt bd in
            let d =
              {   join_locations = js;
                reroot_locations = rs;
                  break_topology = bt;
                     break_delta = bd; }
            in
            join_process_tree t {d
          end

      and join_process_tree t d =
        if EdgeSet.empty join_locations
          then break_process_tree t d
          else begin
            let j1,j2,nd = choose_join d.join_locations d.reroot_locations in
            let jt,jd = Tree.join j1 j2 d.break_topology in
            (Some (jt,jd,d.break_delta)), nd
          end

      let init t = failwith "todo"

      let next d = failwith "TODO"

      let partition d i = failwith "TODO"

      let to_lazy_list d = failwith "todo"

    end

let generate_spr ?join_distance : (module Neighborhood.S) =
  generate_tbr ~join_distance ?reroot_distance:0

let generate_nni : (module Neighborhood.S) =
  generate_tbr ?join_distance:1 ?reroot_distance:0
