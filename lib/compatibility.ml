IFDEF COMPATIBILITY THEN

  let compatibility = true

  external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"
  external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

  let pp_to_string pp_data data =
    let buff = Buffer.create 10 in
    let bufff = Format.formatter_of_buffer buff in
    Format.fprintf bufff "%a@?" pp_data data; (* print AND flush *)
    Buffer.contents buff

ELSE

  let compatibility = false

  let pp_to_string pp_data data =
    Format.asprintf "%a" pp_data data

END

