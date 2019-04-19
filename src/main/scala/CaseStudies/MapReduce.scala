package CaseStudies

import scala.concurrent.Await

object MapReduce {

  import cats.Monoid
  import cats.syntax.monoid._

  def foldMap[A, B: Monoid](vec: Vector[A])(f: A => B) = {
    vec.foldLeft(Monoid[B].empty)((b, a) => b.combine(f(a)))
  }

  import scala.concurrent.Future
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  def parallelFoldMap[A, B: Monoid](vec: Vector[A])(func: A => B): Future[B] = {
    val futures = vec.grouped(Runtime.getRuntime.availableProcessors)
      .map{
        group => Future {
          foldMap(group)(func)
        }
      }
    //futures.toList.sequence.map(iterable => iterable.foldLeft(Monoid[B].empty)(_ |+| _))
    Future.sequence(futures).map(iterable => iterable.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  def parallelFoldMapWithCats[A, B: Monoid](vec: Vector[A])(func: A => B): Future[B] = {
    import cats.instances.future._
    import cats.instances.vector._
    import cats.syntax.foldable._ // for combineAll and foldMap
    import cats.syntax.traverse._ // for traverse
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    vec.grouped(Runtime.getRuntime.availableProcessors())
      .toVector
      .traverse(x => Future(x.foldMap(func)))
      // The call to map combines the match using the combineAll method from Foldable
      .map(_.combineAll)
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.int._ // for Monoid
    println(foldMap(Vector(1, 2, 3))(identity))
    import cats.instances.string._ // for Monoid
    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))

    println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))
    println(Runtime.getRuntime.availableProcessors)
    println((1 to 10).toList.grouped(3).toList)
    println(Await.result(parallelFoldMap(Vector(1, 2, 3,9,8,7,6,8,8,4,5,5,1,11,1234,90,87,66))(identity), 1.second))

    val result: Future[Int] =
      parallelFoldMap((1 to 1000000).toVector)(identity)
    println(Await.result(result, 1.second))


    val future: Future[Int] =
      parallelFoldMapWithCats((1 to 1000).toVector)(_ * 1000)
    println(Await.result(future, 1.second))
  }
}
