open Internal

type 'a llist = Nil | Cons of 'a * ('a llist) Lazy.t

let hd = function Nil -> None | Cons (a,_) -> Some a

let tl = function Nil -> None | Cons (_,b) -> Some !$b

let is_nil = function Nil -> true | Cons _ -> false

let rec const x = Cons (x, lazy (const x))

let singleton a = Cons (a, lazy Nil)

let cons x xs = Cons (x, lazy xs)

let rec of_fn_i f i = Cons (f i, lazy (of_fn f (i+1)))

let rec iterate f p =
  let f_p = f p in
  Cons (f_p, lazy (iterate f f_p))

let from n = of_fn (fun x -> x) n

let rec until p xs = match xs with
  | Nil -> raise Not_found
  | Cons (x,_) when p x -> x
  | Cons (_,xs) -> until p !$xs

let rec fold f xs acc = match xs with
  | Nil -> acc
  | Cons (x,xs) -> fold f (!$xs) (f x acc)

let rec take n xs = match n,xs with
  | 0,_   -> []
  | _,Nil -> []
  | n,Cons (x,xs) -> x :: take (n-1) !$xs

let rec map f xs = match xs with
  | Nil -> Nil
  | Cons (x,xs) -> Cons (f x, lazy (map f !$xs))

let rec weave xs ys = match xs with
  | Nil -> Nil
  | Cons (x,xs) -> Cons(x, lazy (weave ys !$xs))

let rec combine xs ys = match xs,ys with
  | Cons (x,xs), Cons(y,ys) -> Cons ((x,y), lazy (combine !$xs !$ys))
  | _ -> Nil

let rec append xs ys = match xs with
  | Nil -> ys
  | Cons (x,xs) -> Cons (x, lazy (append (!$xs) ys))

let rec filter f xs = match xs with
  | Nil -> Nil
  | Cons (x,xs) when f x -> Cons (x, lazy (filter f !$xs))
  | Cons (_,xs) -> filter f !$xs

let rec thread f t xs = match xs with
  | Nil -> Nil
  | Cons (x,xs) ->
    let t_, x_ = f t x in
    Cons (x_, lazy (map_state f t_ !$xs))

let rec (++) xs ys = append xs ys

let rec concat = function
  | Nil -> Nil
  | Cons (Nil,xss) -> concat !$xss
  | Cons (Cons (x,xs), xss) -> Cons (x, lazy (concat (Cons (!$xs,xss))))

let rec of_stream str =
  try Cons (Stream.next str, lazy (of_stream str))
  with Stream.Failure -> Nil

let rec of_list = function
  | [] -> Nil
  | x::xs -> Cons (x, lazy (of_list xs))

let return a = singleton a

let bind a f = concat $ map f a

