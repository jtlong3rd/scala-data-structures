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
    List().reverse should be ( List() )
    List(1).reverse should be ( List(1) )
    List(1, 2).reverse should be ( List(2, 1) )
    List(1, 2, 3).reverse should be ( List(3, 2, 1) )
    List(1, 2, 3, 4).reverse should be ( List(4, 3, 2, 1) )
  }

  test("foldRight") {
    List().foldRight(0)((x: Int, y: Int) => x + y) should be ( 0 )
    List(1).foldRight(0)(_ + _) should be ( 1 )
    List(1).foldRight(2)(_ + _) should be ( 3 )
    List(1, 2, 3, 4).foldRight(0)(_ + _) should be ( 10 )
    List("Jamie", "Lindsey").foldRight(0)(_.length + _) should be ( 12 )
  }

  test("concatenation operator") {
    List() ++ List() should be ( List() )
    List(1) ++ List() should be ( List(1) )
    List() ++ List(1) should be ( List(1) )
    List() ++ List(1, 2, 3, 4) should be ( List(1, 2, 3, 4) )
    List(1) ++ List(2, 3, 4) should be ( List(1, 2, 3, 4) )
    List(1, 2) ++ List(3, 4) should be ( List(1, 2, 3, 4) )
    List(1, 2, 3) ++ List(4) should be ( List(1, 2, 3, 4) )
    List(1, 2, 3, 4) ++ List() should be ( List(1, 2, 3, 4) )
  }

  test("map") {
    List() map ((x: Int) => -x) should be ( List() )
    List(1) map (-_) should be ( List(-1) )
    List(1, 2) map (-_) should be ( List(-1, -2) )
    List("Jamie", "Lindsey") map (_.length) should be ( List(5, 7) )
    List(1, 2) map (x => List(x, x * 2)) should be ( List(List(1, 2), List(2, 4)) )
  }

  test("flatMap") {
    List(List("Jamie"), List("Lindsey")) flatMap (x => x) should be ( List("Jamie", "Lindsey") )
    List(1, 2) flatMap (x => List(x, x * 2)) should be ( List(1, 2, 2, 4) )
  }

  test("filter") {
    List() filter((x: Int) => x % 2 == 0) should be ( List() )
    List(1, 3, 5) filter (_ % 2 == 0) should be ( List() )
    List(1, 2, 3, 4, 5) filter (_ % 2 == 0) should be ( List(2, 4) )
    List(1, 2, 3, 4, 5) filter (_ % 2 == 0) should be ( List(2, 4) )
  }

  test("reduceLeft") {
    List(1) reduceLeft (_ + _) should be ( 1 )
    List(1, 2) reduceLeft (_ + _) should be ( 3 )
    List(1, 2, 3) reduceLeft (_ + _) should be ( 6 )
    List(1) reduceLeft (_ - _) should be ( 1 )
    List(1, 2) reduceLeft (_ - _) should be ( -1 )
    List(1, 2, 3) reduceLeft (_ - _) should be ( -4 )
  }
}
