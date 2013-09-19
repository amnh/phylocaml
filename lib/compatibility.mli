
val compatibility : bool

IFDEF COMPATIBILITY THEN

  val (@@) : ('a -> 'b) -> 'a -> 'b

  val (|>) : 'a -> ('a -> 'b) -> 'b

END

