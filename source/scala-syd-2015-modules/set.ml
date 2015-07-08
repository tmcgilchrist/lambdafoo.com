module type ORDERED = sig
  type t
  val compare : t -> t -> int
end;;

module type SET = sig
  type elt
  type t
  val empty : t
  val insert : elt -> t -> t
  val member : elt -> t -> bool
end;;

(*
Note: This is an applicative functor, rather than a generative functor.
 *)

module UnbalancedSet (Element : ORDERED) : (SET with type elt = Element.t) = struct
  type elt = Element.t
  type tree = E | T of tree * elt * tree
  type t = tree

  let empty = E

  let rec member x = function
    | E -> false
    | T(a, y, b) when Element.compare x y < 0 -> member x a
    | T(a, y, b) when Element.compare x y > 0 -> member x b
    | _ -> true

  let rec insert x = function
    | E -> T(E, x, E)
    | T(a, y, b) when Element.compare x y < 0 -> T(insert x a, y, b)
    | T(a, y, b) when Element.compare x y > 0 -> T(a, y, insert x b)
    | T(a, y, b) as s -> s
end;;

module IntOrdering = struct
  type t = int

  let compare = Pervasives.compare
end;;

module S = UnbalancedSet(IntOrdering);;

let r = S.(insert 1 empty)
S.(member 1 r)  (* true *)
S.(member 2 r) (* false *)
