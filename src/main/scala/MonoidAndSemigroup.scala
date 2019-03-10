import cats.kernel.Semigroup
import cats.Monoid
import cats.instances.string._
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

object MonoidAndSemigroup {
  def main(args: Array[String]): Unit = {

    //Monoid Instances
    println(Monoid[String].combine("Hi ", "there"))
    println(Monoid[String].empty)

    println(Semigroup[String].combine("Hi ", "there"))

    println(Monoid[Int].combine(32, 10))

    val a = Option(22)
    val b = Option(20)
    println(Monoid[Option[Int]].combine(a, b))

    //Monoid Syntax
    println("Hi " |+| "there" |+| Monoid[String].empty)
    println(1 |+| 2 |+| Monoid[Int].empty)

    //Monoid for sets
    import cats.implicits._

    println(Monoid[Set[Int]].combine(Set(12,23),Set(90,23)))


    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("b" -> 3, "d" -> 4)
    println(map1 |+| map2)
    val tuple1 = ("hello", 123)
    val tuple2 = ("world", 321)
    println(tuple1 |+| tuple2)

    //generic code that works with any type for which we have an instance of Monoid
    def addAll[A](values: List[A])
                 (implicit monoid: Monoid[A]): A =
      values.foldRight(monoid.empty)(_ |+| _)

    println(addAll(List(1, 2, 3)))
    println(addAll(List(None, Some(1), Some(2))))
  }
}
