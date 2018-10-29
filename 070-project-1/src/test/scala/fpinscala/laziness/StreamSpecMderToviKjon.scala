package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import scala.util.Random

import stream00._   
//import stream01._ 
//import stream02._ 

class StreamSpecMderToviKjon extends FlatSpec with Checkers {

  import Stream._

  behavior of "headOption"

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }
  
  it should "return the head of the stream packaged in Some (02)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )
  }

  it should "not force the taild of the stream" in check {
    Prop.forAll {(n: Int) => 
      cons(n, throw new RuntimeException("forced tail")).headOption == Some(n)}
  }
 
  behavior of "take"

  it should "should not force any heads nor any tails of the Stream" in {
    assert(genExceptionStream("forced stream").take(10) != null)
  }

  it should "return empty on empty stream" in {
    forAll((n: Int) => empty.take(n) == empty)
  }

  it should "s.take(n).take(n) == s.take(n) for any Stream s and any n" in check {
    Prop.forAll(genNonEmptyStream[Int]){(s: Stream[Int]) => {
        val N = 100
        s.take(N).take(N).toList == s.take(N).toList
      }}
  }

  behavior of "drop"

  it should "s.drop(n).drop(m) == s.drop(n+m) for any n, m" in check{
    val maxN = 100

    Prop.forAll(from(0).take(maxN)) {(s: Stream[Int]) => {
      val m = Random.nextInt(maxN / 2)
      val n = Random.nextInt(maxN / 2)

      s.drop(m).drop(n).toList == s.drop(m + n).toList
    }}
  }

  it should "not force any of the dropped elements heads or tail" in {
    assert(genExceptionStream("forced stream").drop(10) != null)
  }

  it should "return empty on empty stream" in {
    forAll((n: Int) => empty.drop(n) == empty)
  }

  behavior of "map"

  it should "not change the value if .map(x => x) is used" in check {
    Prop.forAll(genNonEmptyStream[Int]) {(s: Stream[Int]) => {
      s.map(x => x).toList == s.toList
    }}
  }

  it should "terminates on infinite streams" in {
    val value = from(0).map(x => x)
    true
  }

  it should "not break on empty stream" in check {
    Prop.forAll(empty[Int]) {(s: Stream[Int]) => {
      s.map(x => 1).toList == s.toList
    }}
  }

  behavior of "append"

  it should "append stream to stream" in {
    forAll((n: Int) => {
      from(0).take(n).append(from(0).drop(n).take(n)).toList == from(0).take(n * 2)
    })
  }

  // Additional functions tested

  behavior of "toList"
  
  it should "return empty list on empty stream" in {
    forAll((n: Int) => empty.take(n).toList == List())
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
