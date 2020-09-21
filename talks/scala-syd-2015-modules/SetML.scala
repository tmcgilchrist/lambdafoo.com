// This implementation of ML Modules in Scala uses type members.
// In using type parameters we get a similar effect as Generative Functors
// in OCaml.
trait Ordering {
  type T

  def compare(x: T, y: T): Int
}

trait SetSig {
  type Elem
  type Set

  def empty: Set
  def insert(e: Elem, s: Set): Set
  def member(e: Elem, s: Set): Boolean
}

abstract class UnbalancedSet extends SetSig {
  val Element: Ordering
  type Elem = Element.T

  sealed trait Set
  case object Leaf extends Set
  case class Branch(left: Set, elem: Elem, right: Set) extends Set

  val empty = Leaf

  def member(x: Elem, s: Set): Boolean = s match {
    case Leaf => false
    case Branch(l, y, r) =>
      if (Element.compare(x, y) < 0)
        member(x, l)
      else if (Element.compare(x, y) > 0)
        member(x, r)
      else
        true
  }

  def insert(x: Elem, s: Set): Set = s match {
    case Leaf => Branch(Leaf, x, Leaf)
    case Branch(l, y, r) =>
      if (Element.compare(x, y) < 0)
        Branch(insert(x, l), y, r)
      else if (Element.compare(x, y) > 0)
        Branch(l, y, insert(x, r))
      else
        s
  }
}

object IntOrdering extends Ordering {
  type T = Int

  def compare(x: T, y: T): Int = x - y
}

object S extends UnbalancedSet {
  val Element: IntOrdering.type = IntOrdering
}

object R extends UnbalancedSet {
  val Element: IntOrdering.type = IntOrdering
}

var r = S.insert(1, S.empty)
S.member(1, r)
S.member(2, r)
