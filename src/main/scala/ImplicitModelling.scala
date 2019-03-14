import cats.Monoid

object ImplicitModelling {
  implicit class AnImplicit[T:Monoid](value:T) {
    def |-| (anotherValue:T) = {
      //doesn't work
      //implicit val m:Monoid[T] = implicitly[Monoid[T]]

      println("From implicitly-->"+implicitly[Monoid[T]].combine(value,anotherValue))
      import cats.syntax.semigroup._
      //m.combine(value, anotherValue)
      value |+| anotherValue
    }
  }

  def M[T:Monoid](value:T) = implicitly[Monoid[T]]

  def main(args: Array[String]): Unit = {
    import cats.instances.int._
    import cats.syntax.semigroup._
    implicit val m = Monoid[Int]
    println(12 |-| 13)
    println(M(12))

    val either : Either[String,Int] = Left("123")
    println(either.right.map(_ + 1))
  }
}
