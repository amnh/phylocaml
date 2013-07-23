(** Main Functor Modules **)
module Diagnosis    = Diagnosis
module Neighborhood = Neighborhood
module Network      = Network
module Node         = Node
module NodeData     = NodeData
module PTopology    = PTopology
module Topology     = Topology
module Tree         = Tree


(** Phylocaml Core Utility Library *)

module Alphabet     = Alphabet
module Seq          = Seq
module Mlmodel      = Mlmodel
module Llist        = Llist


(** NodeData modules implemented with OCaml data-types. These methods may be
   slower, but used for verification on speed-improvements against C types *)
module NodeDataOCaml = struct
(*   module Likelihood = Likelihood_o *)
(*   module NonAdditive = NonAdditive_o *)
(*   module Sequence = Sequence_o *)
end


(** NodeData modules implemented on with C abstract Types *)
module NodeDataC = struct
(*   module Likelihood = Likelihood_c *)
(*   module Sequence = Sequence_c *)
end
