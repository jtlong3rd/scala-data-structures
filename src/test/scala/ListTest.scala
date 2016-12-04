import org.scalatest.{FunSuite, Matchers}

class ListTest extends FunSuite with Matchers {
  test("apply") {
    List() should be ( Nil() )
    List(1) should be ( Cons(1, Nil()) )
    List(1, 2) should be ( Cons(1, Cons(2, Nil())) )
    List(1, 2, 3) should be ( Cons(1, Cons(2, Cons(3, Nil()))) )
  }
}
