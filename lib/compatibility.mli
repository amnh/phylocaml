
val compatibility : bool

val pp_to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

IFDEF COMPATIBILITY THEN

  val (@@) : ('a -> 'b) -> 'a -> 'b

  val (|>) : 'a -> ('a -> 'b) -> 'b

END

