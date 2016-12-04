sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](args: A*): List[A] = (args foldRight (Nil: List[A])) ( Cons(_, _) )
}
