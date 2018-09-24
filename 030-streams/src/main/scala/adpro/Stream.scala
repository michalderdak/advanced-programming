// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package adpro

sealed trait Stream[+A] {
  import Stream._

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }


  //Exercise 2
  def toList: List[A] = {
    def loop(stream: Stream[A], listToReturn: List[A]): List[A] = {
      if (stream.headOption() != None) {
        val updatedList = listToReturn :+ stream.headOption().head //:+ returns a new list with added element
        loop(stream.tail, updatedList) //Tail is the rest of the list.
      }
      else {
        listToReturn
      }
    }

    loop(this, List())
  }

  //Exercise 3
  def take(n: Int): Stream[A] = {
    def loop(counter: Int, stream: Stream[A]): Stream[A] = stream match {
      case Cons(head, _) => { // head == this.head
        if (counter > 1) {
          cons(head(), loop(counter - 1, stream.tail))
        }
        else if (counter == 1) {
          cons(head(), empty)
        }
        else {
          empty
        }
      }
    }
    loop(n, this)
  }

  def drop(n: Int): Stream[A] = {
    def loop(counter: Int, stream: Stream[A]): Stream[A] = {
      if (counter < n) {
        loop(counter + 1, stream.tail) // We skip by only passing the tail
      }
      else
        stream // We return the rest of the stream
    }

    loop(0, this)
  }
  
  //Exercise 4
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  //Exercise 5
  def forAll(p: A => Boolean): Boolean = {
    def loop(stream: Stream[A]): Boolean = {
      if (stream.headOption() != None) {
        if (p(stream.headOption().head)) {
          loop(stream.tail)
        }
        else {
          false
        }
      }
      else {
        true
      }
    }

    loop(this)
  }

  //Exercise 6
  def takeWhile2(p: A => Boolean): Stream[A] = {
    this.foldRight(empty[A])((head, tail) => {
      if (p(head)) {
        cons(head, tail)
      }
      else {
        empty
      }
    })
  }

  //Exercise 7
  def headOption2 () :Option[A] = {
    foldRight(None: Option[A])((h, t) => Some(h))
  } 

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if(f(h)) cons(h, t) else t)
  }

  def append[B>:A] (s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }
  
  def flatMap[B] (f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h).append(t))
  }

  //Exercise 09
  // def find(p: A => Boolean): Option[A] = { 
  //  this.filter(p).headOption
  // }
  // Put your answer here: As streams are lazy and computed only when needed as oppose to lists, 
  // the filter function won't iterate through the whole collection, but only through elemets until 
  // it finds first suitable element

  //Exercise 10
  //Put your answer here:
  def fibs(): Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, go(n2, n1 + n2))
    }

    go(0, 1)
  }

  //Exercise 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => Empty
    }
  }

  //Exercise 12
  def fibs1(): Stream[Int] = {
    unfold(0, 1){case (x, y) => Some(x, (y, x + y))}
  }

  def from1(n: Int): Stream[Int] = {
    unfold(n)(n => Some(n, (n + 1)))
  }

  //Exercise 13
  def map2[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    } 
  }

  def take2(n: Int): Stream[A] = {
    unfold(this){
      case Cons(h, t) if (n > 1) => Some(h(), t().take2(n - 1))
      case Cons(h, _) if (n == 1) => Some(h(), empty)
      case _ => None
    }

  }
  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if(p(h())) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith2[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  //Exercise 1
  def from(n:Int):Stream[Int]=cons(n,from(n+1))

  def to(n:Int):Stream[Int]= cons(n,from(n-1))

  val naturals: Stream[Int] = from(0)


}

