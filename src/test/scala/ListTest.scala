import org.scalatest.{FunSuite, Matchers}

class ListTest extends FunSuite with Matchers {
  test("apply") {
    List() should be ( Nil )
    List(1) should be ( Cons(1, Nil ) )
    List(1, 2) should be ( Cons(1, Cons(2, Nil)) )
    List(1, 2, 3) should be ( Cons(1, Cons(2, Cons(3, Nil))) )
  }

  test("foldLeft") {
    List().foldLeft(0)((x: Int, y: Int) => x + y) should be ( 0 )
    List(1).foldLeft(0)(_ + _) should be ( 1 )
    List(1).foldLeft(2)(_ + _) should be ( 3 )
    List(1, 2, 3, 4).foldLeft(0)(_ + _) should be ( 10 )
    List("Jamie", "Lindsey").foldLeft(0)(_ + _.length) should be ( 12 )
  }

  test("reverse") {
    List().reverse() should be ( List() )
    List(1).reverse() should be ( List(1) )
    List(1, 2).reverse() should be ( List(2, 1) )
    List(1, 2, 3).reverse() should be ( List(3, 2, 1) )
    List(1, 2, 3, 4).reverse() should be ( List(4, 3, 2, 1) )
  }

  test("foldRight") {
    List().foldRight(0)((x: Int, y: Int) => x + y) should be ( 0 )
    List(1).foldRight(0)(_ + _) should be ( 1 )
    List(1).foldRight(2)(_ + _) should be ( 3 )
    List(1, 2, 3, 4).foldRight(0)(_ + _) should be ( 10 )
    List("Jamie", "Lindsey").foldRight(0)(_.length + _) should be ( 12 )
  }
}
