package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import scala.util.Random

//import stream00._   
//import stream01._ 
import stream02._ 

class StreamSpecMderToviKjon extends FlatSpec with Checkers {

  import Stream._

  behavior of "headOption"

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  it should "return the head of the stream packaged in Some (02) " in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
       ("singleton" |:
         Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
       ("random" |:
         Prop.forAll { (s :Stream[Int]) => s.headOption != None } )
    }

  it should "not force the tail of the stream" in check {
    Prop.forAll {(n: Int) => 
      cons(n, throw new RuntimeException("forced tail")).headOption == Some(n)}
  }
 
  behavior of "take"

  it should "should not force any heads nor any tails of the Stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s: Stream[Int], n: Int) => if(n > 0) s.map(x => x / 0).take(n); true }
  }

  it should "not force the n + 1 element even if we are force all elements of take(n)" in check { 
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
     Prop.forAll{ (s :Stream[Int], n: Int, n1: Int, n2: Int) => cons(n1, cons(n2, cons(n, s.map(x => x / 0)))).take(3); true }
   }

  it should "return empty on empty stream" in {
    forAll((n: Int) => empty.take(n) == empty)
  }

  it should "s.take(n).take(n) == s.take(n) for any Stream s and any n" in check { 
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
     Prop.forAll{ (s :Stream[Int], n: Int) => (n > 0) ==> ((s.take(n)).take(n).toList equals (s.take(n)).toList) } 
   }

  behavior of "drop"

  it should "s.drop(n).drop(m) == s.drop(n+m) for any n, m" in check{
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int]) 
    (""|: Prop.forAll(from(0).take(100)) {(s: Stream[Int]) => {
      val l1 = s.drop(10).drop(10).toList
      val l2 = s.drop(20).toList

      l1 == l2
    }})
  }

  it should "not force any of the dropped elements heads or tail" in {
    assert(genExceptionStream("head was forced").drop(10) != null)
  }

  it should "return empty on empty stream" in {
    forAll((n: Int) => empty.drop(n) == empty)
  }

  behavior of "map"

  it should "not change the value if .map(x => x) is used" in check {
    def infiniteStream :Stream[Int] = cons( 1, infiniteStream)
      Prop.forAll {(n: Int) => infiniteStream.map(x => x) ; true }  
  }

  it should "terminates on infinite streams" in {
    from(0).map(x => x); true
  }

  behavior of "append"

  it should "append stream to stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s1 :Stream[Int], s2 :Stream[Int]) => ((s1.toList).++(s2.toList)) == (s1.append(s2)).toList }
  }

  it should "not force the tail stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s1 :Stream[Int], s2 :Stream[Int]) => s1.append(s2.map(x => x / 0)); true}
  }

  // Helpers

  def list2stream[A] (la: List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]): Gen[Stream[A]] = {
    for { 
      la <- arbitrary[List[A]].suchThat(_.nonEmpty)
    }
    yield list2stream(la)
  }

  def genExceptionStream (s: String): Stream[RuntimeException] = {
    cons(throw new RuntimeException(s), genExceptionStream(s))
  }
}
