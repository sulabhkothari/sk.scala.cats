import java.util.Date

import cats.Show

object Miscellaneous {
  def main(args: Array[String]): Unit = {
    testEquality
    testShow
    testEqualityWithAllCatsImports
  }

//  implicit val dateShow: Show[Date] = {
//    (date: Date) => s"${date.getTime}ms since the epoch."
//  }

  implicit val dateShow2: Show[Date] =
    Show.show(date => s"${date.getTime}ms since the epoch.")

  def testEquality = {
    import cats.Eq
    import cats.instances.int._

    val eqInt = Eq[Int]
    println(  eqInt.eqv(123,345))

    import cats.syntax.eq._
    123 === 123

    import cats.instances.option._
    import cats.syntax.option._ // for some and none
    println("With Option --> " + (1.some === none[Int]))
    println("With Option (None) --> " + (1.some =!= none[Int]))
  }

  def testEqualityWithAllCatsImports = {
    import cats._
    import cats.instances.all._
    val eqInt = Eq[Int]
    println(  eqInt.eqv(123,345))

    import cats.syntax.all._
    123 === 123
  }

  def testShow = {
    import cats.Show
    import cats.instances.int._    // for Show
    import cats.instances.string._ // for Show
    val showInt: Show[Int] = Show.apply[Int]
    val showString: Show[String] = Show.apply[String]
    println(showInt.show(123))
    println(showString.show("abc"))

    import cats.syntax.show._
    println(456.show)

    println(new Date() show)

  }
}
