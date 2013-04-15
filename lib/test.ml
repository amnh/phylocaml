open Phylocaml_pervasives

module Root = Node.Make1D (Tree.NodeComparator) (SetData)
module Node = Node.Make3D (Tree.NodeComparator) (SetData)

module PTree = Ptopology.Make (Tree)
module Diag = Diagnosis.Make (PTree) (Root) (Node)

let diagnose_tree taxa_data edges =
    let process_node_data narray : SetData.t IntMap.t =
        Array.fold_left
            (fun (acc,map) x -> (acc+1,IntMap.add acc x map))
            (0,IntMap.empty)
            narray
        |> snd
    in
    let nodes =
        Array.mapi
            (fun i x ->
                let nd = process_node_data x in
                Node.of_data i nd)
            taxa_data
        |> Array.to_list
    in
    let tree = PTree.of_parsed nodes edges in
    let tree = Diag.diagnose tree in
    Printf.printf "%s:[%d]" (PTree.to_string tree) (Diag.total_cost tree)

