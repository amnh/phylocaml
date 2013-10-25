open Internal

module IDMap = Topology.IDMap
module EdgeMap = Topology.EdgeMap
module HandleMap = Topology.HandleMap

type 'root root_data = {
  assignment : (Topology.jxn * 'root) IntMap.t;
  component_cost : float;
  adjusted_cost : float;
}

type ('node,'root) t = {
  node_data : 'node IDMap.t;
  root_data : 'root EdgeMap.t;
  comp_root : 'root root_data HandleMap.t;
}

let empty = {
  node_data = IDMap.empty;
  root_data = EdgeMap.empty;
  comp_root = HandleMap.empty;
}

let get_node_data a t = IDMap.find a t.node_data
let add_node_data a d t = {t with node_data = IDMap.add a d t.node_data}
let remove_node_data a t = {t with node_data = IDMap.remove a t.node_data}

let get_root_data e t = EdgeMap.find e t.root_data
let add_root_data a d t = {t with root_data = EdgeMap.add a d t.root_data}
let remove_root_data e t = {t with root_data = EdgeMap.remove e t.root_data}

let get_comp_root h t = HandleMap.find h t.comp_root
let add_comp_root h d t = {t with comp_root = HandleMap.add h d t.comp_root}
let remove_comp_root h t = {t with comp_root = HandleMap.remove h t.comp_root}


let get_comp_root_i h i t =
  IntMap.find i (HandleMap.find h t.comp_root).assignment

let add_comp_root_i h i d t =
  let x =
    let y = HandleMap.find h t.comp_root in
    {y with assignment = IntMap.add i d y.assignment}
  in
  {t with comp_root = HandleMap.add h x t.comp_root;}

let remove_comp_root_i h i t =
  let x =
    let y = HandleMap.find h t.comp_root in
    {y with assignment = IntMap.remove i y.assignment}
  in
  {t with comp_root = HandleMap.add h x t.comp_root;}

