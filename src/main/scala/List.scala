sealed trait List[+A] {
  def foldLeft[B](acc: B)(f: (B, A) => B): B = {
    def folder(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(head, tail) => folder(tail, f(acc, head))
    }

    folder(this, acc)
  }
}

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](args: A*): List[A] = (args foldRight (Nil: List[A])) (Cons(_, _))
}
