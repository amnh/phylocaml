IFDEF COMPATIBILITY THEN

  let compatibility = true

  let (@@) a b = a b

  external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

ELSE

  let compatibility = false

END

