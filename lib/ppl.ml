open Internal

type 'a pp_l = 'a -> string

type align = L | C | R

let rec repeat_ c x b = match x with
  | 0 -> ()
  | x -> Buffer.add_string b c;
         repeat_ c (x-1) b

let align_to_string = function
  | L -> "l"
  | C -> "c"
  | R -> "r"

let l_matrix align l_celldata matrix =
  let b = Buffer.create 1789 in
  Buffer.add_string b "\\begin{array}{";
  Array.iter (fun x -> Buffer.add_string b @@ align_to_string x) align;
  Buffer.add_string b "}\n";
  for i = 0 to (Array.length matrix)-1 do
    Buffer.add_string b (l_celldata matrix.(i).(0));
    for j = 1 to (Array.length matrix.(0)) - 1 do
      Buffer.add_string b " & ";
      Buffer.add_string b (l_celldata matrix.(i).(j));
    done;
    Buffer.add_string b " \\\\\n";
  done;
  Buffer.add_string b "\\end{array}";
  Buffer.contents b

let l_table align l_headdata l_celldata (header,table) =
  let b = Buffer.create 1789 in
  Buffer.add_string b "\\begin{array}{";
  Array.iter (fun x -> Buffer.add_string b @@ align_to_string x) align;
  Buffer.add_string b "}\n";
  Buffer.add_string b (l_headdata header.(0));
  for i = 1 to (Array.length header)-1 do
    Buffer.add_string b " & ";
    Buffer.add_string b (l_headdata header.(i));
  done;
  for i = 0 to (Array.length table)-1 do
    Buffer.add_string b (l_celldata table.(i).(0));
    for j = 1 to (Array.length table.(0)) - 1 do
      Buffer.add_string b " & ";
      Buffer.add_string b (l_celldata table.(i).(j));
    done;
    Buffer.add_string b " \\\\\n";
  done;
  Buffer.add_string b "\\end{array}";
  Buffer.contents b

let l_datatable align l_headdata l_coldata l_celldata (header,column,table) =
  let b = Buffer.create 1789 in
  Buffer.add_string b "\\begin{array}{";
  Array.iter (fun x -> Buffer.add_string b @@ align_to_string x) align;
  Buffer.add_string b "}\n";
  Buffer.add_string b (l_headdata header.(0));
  for i = 1 to (Array.length header)-1 do
    Buffer.add_string b " & ";
    Buffer.add_string b (l_headdata header.(i));
  done;
  for i = 0 to (Array.length table)-1 do
    Buffer.add_string b (l_coldata column.(i));
    for j = 0 to (Array.length table.(0)) - 1 do
      Buffer.add_string b " & ";
      Buffer.add_string b (l_celldata table.(i).(j));
    done;
    Buffer.add_string b " \\\\\n";
  done;
  Buffer.add_string b "\\end{array}";
  Buffer.contents b

