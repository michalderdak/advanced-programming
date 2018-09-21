// Andrzej Wąsowski, IT University of Copenhagen

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  /* Exercise 1 */
  def square (n: Int): Int = {
    n*n
  }

  private def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs (x)}"

  val magic :Int = 42
  var result :Option[Int] = None

  def main(args: Array[String]): Unit = {
    assert (magic - 84 == magic.-(84))
    println (formatAbs (magic-100))

    println(fib(1))
  }

  def fib (n: Int) : Int = {
    fibSum(n, -1, 0)
  }

  @annotation.tailrec
  def fibSum (n: Int, a: Int, b: Int) : Int = n match {
    case 0 => a
    case n => { 
        fibSum(n - 1, b, a + b) 
      }
  }
}
