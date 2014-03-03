open Internal

type 'a pp_f = Format.formatter -> 'a -> unit

let measure_width pp_data data : int =
  String.length (pp_to_string pp_data data)

let combine_width max_widths pp_rowdata row =
  Array.iteri
    (fun i _ ->
      max_widths.(i) <- max max_widths.(i) (measure_width pp_rowdata row.(i)))
    row

let pp_array ?(hsep=" ") pp_celldata fmt array =
  Array.iteri
    (fun j cell ->
      let () = if j <> 0 then Format.pp_print_string fmt hsep in
      Format.fprintf fmt "%a@," pp_celldata cell)
    array;
  ()

let pp_matrix ?(hsep=" ") pp_celldata fmt matrix =
  let widths =
    let acc = Array.make (Array.length matrix.(0)) 0 in
    Array.iter (fun x -> combine_width acc pp_celldata x) matrix;
    acc
  in
  Format.pp_open_tbox fmt ();
  for j = 0 to pred @@ Array.length matrix.(0) do
    let lsep = if j = 0 then "" else hsep in
    Format.pp_set_tab fmt ();
    (* In the following ' *' is not supported with the %a argument...
    Format.fprintf fmt "%s% *a" lsep widths.(j) pp_celldata matrix.(0).(j); *)
    Format.fprintf fmt "%s% *s" lsep widths.(j) (string_of_pp pp_celldata matrix.(0).(j));
  done;
  for i = 1 to pred @@ Array.length matrix do
    for j = 0 to pred @@ Array.length matrix.(i) do
      let lsep = if j = 0 then "" else hsep in
      Format.pp_print_tab fmt (); (* print_tab *)
      (* Format.fprintf fmt "%s% *a" lsep widths.(j) pp_celldata matrix.(i).(j) *)
      Format.fprintf fmt "%s% *s" lsep widths.(j) (string_of_pp pp_celldata matrix.(i).(j));
    done;
  done;
  Format.pp_close_tbox fmt ()

let pp_table ?(hsep=" ") pp_headdata pp_celldata fmt (header,table) =
  let widths =
    let acc = Array.make (Array.length table.(0)) 0 in
    combine_width acc pp_headdata header;
    Array.iter (fun x -> combine_width acc pp_celldata x) table;
    acc
  in
  Format.pp_open_tbox fmt ();
  for j = 0 to pred @@ Array.length header do
    let lsep = if j = 0 then "" else hsep in
    Format.pp_set_tab fmt ();
    (* In the following ' *' is not supported with the %a argument...
    Format.fprintf fmt "%s% *a" lsep widths.(j) pp_headdata header.(j); *)
    Format.fprintf fmt "%s% *s" lsep widths.(j) (string_of_pp pp_headdata header.(j));
  done;
  for i = 0 to pred @@ Array.length table do
    for j = 0 to pred @@ Array.length table.(i) do
      let lsep = if j = 0 then "" else hsep in
      Format.pp_print_tab fmt ();
      (* Format.fprintf fmt "%s% *a" lsep widths.(j) pp_celldata table.(i).(j) *)
      Format.fprintf fmt "%s% *s" lsep widths.(j) (string_of_pp pp_celldata table.(i).(j));
    done;
  done;
  Format.pp_close_tbox fmt ()

