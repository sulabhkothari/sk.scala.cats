object PartialUnification {
  def main(args: Array[String]): Unit = {
    import cats.Functor
    import cats.instances.function._
    import cats.syntax.functor._
    val func1 = (x: Int) => x.toDouble
    val func2 = (y: Double) => y * 2
    val func3 = func1.map(func2)

    val func3a: Int => Double =
      a => func2(func1(a))
    val func3b: Int => Double =
      func2.compose(func1)

    println(func3(10))
    println(func3b(100))

    import cats.syntax.contravariant._
    //func2.contramap(func1)
    // Hypothetical example. This won't actually compile:
    //    val func3c: Int => Double =
    //      func2.contramap(func1)

    type <=[B, A] = A => B
    //type F[A] = Double <= A

    val func2b: Double <= Double = func2
    val func3c = func2b.contramap(func1)

    println(func3c(99))
  }
}
