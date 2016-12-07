import scala.annotation.tailrec

// The List trait is "sealed" so as to ensure that
// it cannot be extended by anything other than
// the two case classes ("patterns") below
sealed trait List[+A] {
  def foldLeft[B](acc: B)(f: (B, A) => B): B = {
    @tailrec
    def folder(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(head, tail) => folder(tail, f(acc, head))
    }

    folder(this, acc)
  }

  def reverse(): List[A] = {
    @tailrec
    def reverser(left: List[A], reversed: List[A]): List[A] = left match {
      case Nil => reversed
      case Cons(head, tail) => reverser(tail, Cons(head, reversed))
    }

    reverser(this, Nil)
  }

  def foldRight[B](acc: B)(f: (A, B) => B): B = (this.reverse foldLeft acc) (
    (a, b) => f(b, a)
  )
}

// In English, a list can be built up by starting will
// Nil (the empty list) and repeatedly appending
// new elements to the front of a preexisting list
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// This is the "companion object" for the List trait
// All of our static/factory methods will placed here
object List {
  def apply[A](args: A*): List[A] = (args foldRight (Nil: List[A])) (Cons(_, _))
}
