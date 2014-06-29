module type Annotate =
  sig
    type s
    type m
    type 'a t

    val annotate : m -> s -> s -> s t * s t
  
  end

module type Align =
  sig
    type t
    type m
    type c

    val distance : m -> t -> t -> c

    val median : m -> t -> t -> c * t

  end
