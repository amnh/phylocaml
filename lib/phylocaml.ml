(** Main Functor Modules **)
module Diagnosis    = Diagnosis
module Neighborhood = Neighborhood
module Node         = Node
module NodeData     = NodeData
module PTopology    = Ptopology
module Topology     = Topology

(** Tree Oriented Modules *)
module Tree         = Tree
(* module DiagTree     = DiagTree *)

(** Network Oriented Modules *)
module Network      = Network
(* module DiagNetwork  = DiagNetwork *)

(** Model Impelementations *)
(* module Model        = Model *)
module MlModel      = MlModel
(* module Mpmodel      = Mpmodel *)
(* module Kolmomodel   = Kolmomodel *)

(** Phylocaml Core Utility Library *)
module Alphabet     = Alphabet
module Sequence     = Sequence
module Llist        = Llist
module Bitvector    = Bitvector

(** NodeData modules implemented with OCaml data-types. *)
module NodeDataOCaml =
  struct
    module Likelihood  = Likelihood_o
    module NonAdditive = NonAdditive_o
    module Sequence    = Sequence_o
  end

(** NodeData modules implemented on with C abstract Types. At least a subset of
   the NodeDataOCaml module. *)
module NodeDataC =
  struct
    module Likelihood  = Likelihood_c
    module NonAdditive = NonAdditive_c
    module Sequence    = Sequence_c
  end
