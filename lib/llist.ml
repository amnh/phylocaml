open Phylocaml_pervasives

type 'a llist = Nil | Cons of 'a * ('a llist) Lazy.t

let rec take f n xs =

let rec map f xs =

let rec const x = Cons (x, lazy (const x))

let rec combine xs ys =

let rec filter f xs =

let rec concat xss =

let rec of_state f t i =
  let global_t = ref t in
  let global_f = (fun s -> f !global_t s) in
  Cons (


let hd = function Nil -> None | Cons (a,_) -> Some a

let tl = function Nil -> None | Cons (_,b) -> Some (!$ b)

let is_empty = function Nil -> true | Cons _ -> false
