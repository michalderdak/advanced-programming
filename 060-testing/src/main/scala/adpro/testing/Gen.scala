// Advanced Programming 2015-2018
// Wasowski, Fu. IT University of Copenhagen
//
// Search for Exercise 1 to find the starting point

// This file is best worked on by reading exercise descriptions in the
// corresponding PDF file.  Then find the place in the file where the stub for
// the exercise is created and complete it.
//
// Before starting to work on the exercises, familiarize yourself with the the
// content already in the file (it has been explained in chapter 8, but it is
// useful to see it all together in one file).

package adpro.testing
import fpinscala.state._
import fpinscala.state.RNG._

// A generator will use a random number generator RNG in its state, to create
// random instances (but perhaps also some other staff)
case class Gen[A] (sample :State[RNG,A]) {

  // Let's convert generator to streams of generators
  def toStream (seed: Long): Stream[A] =
    Gen.state2stream (this.sample) (RNG.Simple (seed))
  def toStream (rng: RNG): Stream[A] =
    Gen.state2stream (this.sample) (rng)

  // Exercise 3

  def listOfN (n: Int): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(sample)))
  }

  // Exercise 4

  def flatMap[B] (f: A => Gen[B]): Gen[B] = {
    Gen(this.sample.flatMap(a => f(a).sample))
  }

  // It would be convenient to also have map  (uses flatMap)

  def map[B] (f: A => B): Gen[B] = this.flatMap (a => Gen.unit[B] (f(a)))

  // Exercise 5

  def listOfN (size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(a => listOfN(a))
  }

  // Exercise 6

  def union (that: Gen[A]): Gen[A] = {
    Gen.boolean.flatMap(coin => if(coin) this else that)
  }

  // Exercise 7 continues in the companion object (below)
}

object Gen {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => n #:: state2stream (s) (s1) }

  // A generator for Integer instances

  def anyInteger: Gen[Int] = Gen(State(_.nextInt))

  // Exercise 1

  def choose (start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  // Exercise 2

  def unit[A] (a: =>A): Gen[A] = {
    Gen(State(RNG.unit(a)))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def double: Gen[Double] = {
    Gen(State(RNG.double))
  }

  // (Exercise 3 is found in the Gen class above)

}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result { def isFalsified: Boolean }
  case object Passed extends Result { def isFalsified = false }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
      def isFalsified = true
  }
  case object Proved extends Result { def isFalsified = false }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => as.toStream(rng).zip(Stream.from(0)).take(n).map {
      case (a,i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import Prop._

case class Prop (run: (TestCases,RNG)=>Result) {

  // (Exercise 7)

  def && (that: Prop): Prop = Prop { 
    (testCase, rng) => run(testCase, rng) match {
      case Passed => that.run(testCase, rng)
      case Proved => that.run(testCase, rng)
      case Falsified(failure, successes) => Falsified(failure, successes)
    }
   }

  def || (that: Prop): Prop = Prop { 
    (testCase, rng) => run(testCase, rng) match {
      case Falsified(failure, successes) => that.run(testCase, rng)
      case anythingElse => anythingElse
    }
  }

}

// vim:cc=80:foldmethod=indent:nofoldenable
