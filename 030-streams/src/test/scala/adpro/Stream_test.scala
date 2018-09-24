package adpro
import org.scalatest.FunSuite
import Stream._

class Stream_test extends FunSuite {
  
  test("This one always works: (-1) * (-1) = 1") {
    assert((-1)*(-1)==1);
  }
  
  //Sanity check of exercise 1
  test("The first element of Stream.from(3) is 3") {
    assert(from(3).headOption().contains(3));
  }

  test("The second element of Stream.from(3) is 4") {
    assert(from(3).tail.headOption().contains(4));
  }

	test("The first element of Stream.to(3) is 3") {
    assert(to(3).headOption().contains(3));
  }
  
	test("The second element of Stream.to(3) is 2") {
    assert(to(3).tail.headOption().contains(2));
  }

	test("The first element of naturals is 0"){
		assert(naturals.headOption().contains(0))
	}


	test("The Stream(1,2,3).toList is List(1,2,3) "){
		  val l2 :Stream[Int]= cons(1, cons(2, cons (3, empty)))
			assert(l2.toList(0) == 1)
			assert(l2.toList(1) == 2)
			assert(l2.toList(2) == 3)


	}

	test("naturals.take(3)  is Stream(1,2,3) "){
		  val l2 :Stream[Int]= naturals.take(3) 
			assert(l2.toList(0) == 0)
			assert(l2.toList(1) == 1)
			assert(l2.toList(2) == 2)
  }

  test("naturals.drop(3) is Stream(3,4,5,...) "){
		  val l2 :Stream[Int]= naturals.drop(3) 
			assert(l2.headOption().contains(3))
      assert(l2.tail.headOption().contains(4))
      assert(l2.tail.tail.headOption().contains(5))

			//assert(l2.toList(2) == 5)
  }
	
  test ("Exercise 4") {
      val l = naturals.takeWhile(_<1000000000).drop(100).take(3)
      assert(l.headOption().contains(100))
      assert(l.tail.headOption().contains(101))
      assert(l.tail.tail.headOption().contains(102))
    }

  test("Exercise 7 test 1") {
    assert(from(3).headOption2().contains(3));
  }

  test("Exercise 7 test 2") {
    assert(from(3).tail.headOption2().contains(4));
  }

	test("Exercise 7 test 3") {
    assert(to(3).headOption2().contains(3));
  }
  
	test("Exercise 7 test 4") {
    assert(to(3).tail.headOption2().contains(2));
  }

	test("Exercise 7 test 5"){
		assert(naturals.headOption2().contains(0))
	}

  test("Exercise 8 map") {
    val l = naturals.map(_ * 2).drop(30).take(3).toList
    
    assert(l(0) == 60)
    assert(l(1) == 62)
    assert(l(2) == 64)
  }

  test("Exercise 8 filter") {
    val l = naturals.drop(10).filter(_ % 2 == 0).take(10).toList

    assert(l(0) == 10)
    assert(l(1) == 12)
    assert(l(2) == 14)
    assert(l(3) == 16)
    assert(l(4) == 18)
    assert(l(5) == 20)
  }

  test("Exercise 8 append") {
    naturals.append (naturals)
    val l = naturals.take(1)
    val l2 = naturals.take(1).append(l).toList
    
    assert(l2(0) == 0)
    assert(l2(1) == 0)
  }

  test("Exercise 8 flatMap") {
    val l = naturals.flatMap(x => from(x + 1)).take(3).toList
    
    assert(l(0) == 1)
    assert(l(1) == 2)
    assert(l(2) == 3)
  }

  test("Exercise 10 fib") {
    val l = naturals.fibs(0, 1).take(7).toList

    assert(l(0) == 0)
    assert(l(1) == 1)
    assert(l(2) == 1)
    assert(l(3) == 2)
    assert(l(4) == 3)
    assert(l(5) == 5)
    assert(l(6) == 8)
  }

  test("Exercise 11") {
    val l = naturals.take(10).toList
    val l2 = naturals.unfold[Int, Int](0)(x => if(x < 10) Some(x, x + 1) else None).toList

    assert(l == l2)
  }
    
}