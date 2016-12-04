sealed trait List[A]

case class Nil[A]() extends List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](args: A*): List[A] = (args foldRight (Nil(): List[A])) ( Cons(_, _) )
}
