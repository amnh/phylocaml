(** {1 Model}
    The Generalization of a model. Cearly this is an ultimate generalization at
    this point, and may need to be refined, although current thinking is that
    only a few modules will use t, but many will pass that value along.

     Diagnosis --> Node --> NodeData

    It is currently presumed that Diagnosis and NodeData would have
    understanding of the model. The Diagnosis module needs the information for
    the 'model_fn' in the optimization, and NodeData needs it for the defining
    a median, distance, and general assignments. *)

module type S = 
  sig
    type t
  end
