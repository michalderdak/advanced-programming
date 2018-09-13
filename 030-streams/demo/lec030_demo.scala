
object Lec40{
  // Helper function to measure the time for an evaluation
  // Usage time{println("Hello World")}
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }

// Function that determines whether n is a prime number
def isPrime(n: Int) = (n>=2) && ! ((2 until n-1) exists (n % _ == 0))


  //Demo: Performance issue -- How to get the 31-st prime
  time{(1 to 1000).filter(isPrime)(30)}
  time{(1 to 1000).toStream.filter(isPrime)(30)}

  //Demo: Quiz on different evaluation strategies
  val myexpression = { println()
    val hello = {println("hello");5}
    lazy val bonjour={println("bonjour");7}
    def hej={println("hej");3}
    hej+bonjour+hello+hej+bonjour+hello
  }
     //hello, hej, bonjour hej, 30

  //Demo: strictness/non-strictness
  true || sys.error("fail")
  false && sys.error("fail")
  def hej_strict (x:Int) = {println("hej");0}
  def hej_nonstrict(x: =>Int)={println("hej");0}
  hej_strict(sys.error("fail"))
  hej_nonstrict(sys.error("fail"))

  //Demo: Stream Implementation  
  sealed trait Stream[+A]
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h:  A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      val head = hd
      lazy val tail = tl
      Cons(head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }


  def get[A](n:Int,s:Stream[A]):A= s match {
    case Cons(hd,tl)=> if (n==0) hd else get(n-1, tl())
    case _ => throw new Exception("out of bound!")
  }

  def filter[A](p: A => Boolean, s:Stream[A]): Stream[A] = s match{
    case Cons(hd,tl) =>
      if (p(hd)) Stream.cons(hd, filter(p,tl())) else filter(p,tl())
    case _ =>Stream.empty
  }

  def streamRange(lo: Int, hi: Int): Stream[Int] = {
     if(lo >= hi) Stream.empty
     else Stream.cons(lo, streamRange(lo + 1, hi))
  }

//compare again the performance
  time{get(30, filter(isPrime,streamRange(1,1000)))}
  time{(1 to 1000).filter(isPrime)(30)}
  time{(1 to 1000).toStream.filter(isPrime)(30)}

  //Infinite list
  val ones: Stream[Int] = Stream.cons(1, ones)
  scala.collection.immutable.Stream.from(2)
  val TheStream=collection.immutable.Stream

}