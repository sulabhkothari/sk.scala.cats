import cats.Monoid

trait Combiner[A] {
  def combine(a1: A, a2: A): A
}

object Combiner{
  implicit val IntCombiner = new ComplexCombiner[Int] {
    override def combine(a1: Int, a2: Int): Int = {
      println("CC2")
      a1 + a2
    }
  }
}

trait ComplexCombiner[A] extends Combiner[A]

object ComplexCombiner {
  implicit val IntComplexCombiner = new ComplexCombiner[Int] {
    override def combine(a1: Int, a2: Int): Int = {
      println("CC1")
      a1 * a2
    }
  }
}

case class Mapp[K, V](val k: K, val v: V)

object MappInstances {
  implicit def mapCombiner[K, V](implicit cc: Combiner[V]) = new Combiner[Mapp[K, V]] {
    override def combine(a1: Mapp[K, V], a2: Mapp[K, V]): Mapp[K, V] = Mapp(a1.k, cc.combine(a1.v, a2.v))
  }
}

object MappSyntax {

  implicit class MappDecorator[K, V](m: Mapp[K, V])(implicit cc: Combiner[Mapp[K, V]]) {
    def combine(m2: Mapp[K, V]) = {
      cc.combine(m, m2)
    }
  }

}

import MappSyntax._

trait CombineMapps {
  def cMapp(m1: Mapp[Int, Int], m2: Mapp[Int, Int])(implicit cc: ComplexCombiner[Int]): Mapp[Int, Int]
}

import MappInstances._
object CombineMapps {
  def approach1 = new CombineMapps {
    override def cMapp(m1: Mapp[Int, Int], m2: Mapp[Int, Int])(implicit cc: ComplexCombiner[Int]) = {
      m1 combine m2
    }
  }

    def approach2(implicit m: Combiner[Mapp[Int,Int]]) = new CombineMapps {
      override def cMapp(m1: Mapp[Int, Int], m2: Mapp[Int, Int])(implicit cc: ComplexCombiner[Int]) = m1 combine m2
    }
}

trait ImplicitTest[A] {
  def test: A
}

object ImplicitTest {
  implicit val theValue: ImplicitTest[Int] = new ImplicitTest[Int] {
    override def test: Int = 10
  }
}

object ImplicitModelling {

  implicit class AnImplicit[T: Monoid](value: T) {
    def |-|(anotherValue: T) = {
      //doesn't work
      //implicit val m:Monoid[T] = implicitly[Monoid[T]]

      println("From implicitly-->" + implicitly[Monoid[T]].combine(value, anotherValue))
      import cats.syntax.semigroup._
      //m.combine(value, anotherValue)
      value |+| anotherValue
    }
  }

  def M[T: Monoid](value: T) = implicitly[Monoid[T]]

  def main(args: Array[String]): Unit = {
    import cats.instances.int._
    implicit val m = Monoid[Int]
    println(12 |-| 13)
    println(M(12))

    val either: Either[String, Int] = Left("123")
    println(either.right.map(_ + 1))

    println(implicitly[ImplicitTest[Int]].test)

    println(CombineMapps.approach1.cMapp(Mapp(12, 12), Mapp(9, 9)))
    println(CombineMapps.approach2.cMapp(Mapp(12, 12), Mapp(9, 9)))
  }
}
