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

  def reverse: List[A] = {
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

  def ++[B >: A](right: List[B]): List[B] = {
    @tailrec
    def concatter(right: List[B], reversedAcc: List[B]): List[B] = right match {
      case Nil => reversedAcc.reverse
      case Cons(head, tail) => concatter(tail, Cons(head, reversedAcc))
    }

    concatter(right, this.reverse)
  }

  def map[B](f: A => B): List[B] = (this foldRight (Nil: List[B])) ((next, mapped) => Cons(f(next), mapped))

  def flatMap[B](f: A => List[B]): List[B] = (this foldRight (Nil: List[B])) ((next, mapped) => f(next) ++ mapped)

  def filter(f: A => Boolean): List[A] = (this foldRight (Nil: List[A])) (
    (next, filtered) => if (f(next)) Cons(next, filtered) else filtered
  )

  def reduceLeft[B >: A](f: (B, B) => B): B = this match {
    case Nil => throw new Error("Can't reduce an empty list!")
    case Cons(head, tail) => (tail foldLeft (head: B)) (f)
  }

  def reduceRight[B >: A](f: (B, B) => B): B = this.reverse reduceLeft (
    (b1: B, b2: B) => f(b2, b1)
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
