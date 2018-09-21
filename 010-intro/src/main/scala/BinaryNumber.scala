trait Option[+A] {
    def map[B] (f: A => B) : Option[B]
    def flatMap[B] (f: A => Option[B]) : Option[B]
    def filter (f: A => Boolean) : Option[A]
    def getOrElse[A] (default: A) : A
}

class Name[A] extends Option[A] {
    def filter(f: A => Boolean): Option[A] = ???
    def flatMap[B](f: A => Option[B]): Option[B] = ???
    def getOrElse[A](default: A): A = ???
    def map[B](f: A => B): Option[B] = ???
}