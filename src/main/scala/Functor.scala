
object FunctorExamples {

  def main(args: Array[String]): Unit = {
    futuresAsFunctor
    futuresAsFunctorsWithSideEffects

    functionSequencing
  }

  def futuresAsFunctor = {
    import scala.concurrent.{Future, Await}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    val future: Future[String] =
      Future(123).
        map(n => n + 1).
        map(n => n * 2).
        map(n => n + "!")
    println(Await.result(future, 1.second))
  }

  def futuresAsFunctorsWithSideEffects = {
    import scala.util.Random
    import scala.concurrent.{Future, Await}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    val future1 = {
      // Initialize Random with a fixed seed:
      val r = new Random(0L)
      // nextInt has the side-effect of moving to
      // the next random number in the sequence:
      val x = Future(r.nextInt)
      for {
        a <- x
        b <- x
      } yield (a, b)
    }
    val future2 = {
      val r = new Random(0L)
      for {
        a <- Future(r.nextInt)
        b <- Future(r.nextInt)
      } yield (a, b)
    }
    val result1 = Await.result(future1, 1.second)
    val result2 = Await.result(future2, 1.second)

    println(result1)
    println(result2)
  }

  def functionSequencing = {

    val func1: Int => Double =
      (x: Int) => x.toDouble

    val func2: Double => Double =
      (y: Double) => y * 2

    //Add scalacOptions += "-Ypartial-unification" to build.sbt
    import cats.instances.function._ // for Functor
    import cats.syntax.functor._ // for map
    println((func1 map func2) (1))

    println((func1 andThen func2) (1))


    println(func2(func1(1))) // composition written out by hand

    val func =
      ((x: Int) => x.toDouble).
        map(x => x + 1).
        map(x => x * 2).
        map(x => x + "!")

    println(func(10))
  }
}

object SimplifiedFunctorDefinition {

  import scala.language.higherKinds

  trait Functor1[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  import cats.Functor

  implicit class FunctorOps[F[_], A](src: F[A]) {
    def map[B](func: A => B)
              (implicit functor: Functor[F]): F[B] =
      functor.map(src)(func)
  }

  //  Sometimes we need to inject dependencies into our instances. For example, if we had to define a custom Functor for
  //  Future (another hypothetical example—Cats provides one in cats.instances.future) we would need to account for the
  //  implicit ExecutionContext parameter on future.map. We can’t add extra parameters to functor.map so we have to account
  //  for the dependency when we create the instance:
  import scala.concurrent.{Future, ExecutionContext}
  implicit def futureFunctor
  (implicit ec: ExecutionContext): Functor[Future] =
    new Functor[Future] {
      def map[A, B](value: Future[A])(func: A => B): Future[B] =
        value.map(func)
    }
  //  Whenever we summon a Functor for Future, either directly using Func- tor.apply or indirectly via the map extension method,
  //  the compiler will locate futureFunctor by implicit resolution and recursively search for an ExecutionContext at the call site
}

object CatsFunctors {

  import cats.Functor

  def main(args: Array[String]): Unit = {
    catsInstances
    functorsOnFunctions

    import cats.instances.option._ // for Functor
    import cats.instances.list._ // for Functor
    println(doMath(Option(20)))
    println(doMath(List(1, 2, 3)))
  }

  def catsInstances = {
    import scala.language.higherKinds
    import cats.Functor
    import cats.instances.list._ // for Functor
    import cats.instances.option._ // for Functor
    val list1 = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)
    val option1 = Option(123)
    val option2 = Functor[Option].map(option1)(_.toString)
    println(option2)

    // Lift Method
    val func = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)
    println(liftedFunc(Option(1)))
  }

  def functorsOnFunctions = {
    import cats.instances.function._ // for Functor
    import cats.syntax.functor._ // for map
    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a * 2
    val func3 = (a: Int) => a + "!"
    val func4 = func1.map(func2).map(func3)
    println(func4(123))
  }

  //The main method provided by the syntax for Functor is map. It’s difficult to demonstrate this with Options and Lists as they have their own built-in map methods and the Scala compiler will always prefer a built-in method over an extension method. We’ll work around this with two examples.
  //This ti􏰀me we’ll abstract over functors so we’re not working with any parti􏰀cular concrete type
  def doMath[F[_]](start: F[Int])
                  //This implicit is required to bring Functor[F] into implicit scope, for FunctorOps - map requiring this implicit. Refer to SimplifiedFunctorDefinition object.
                  (implicit functor: Functor[F]): F[Int] = {
    import cats.syntax.functor._ // for map
    start.map(n => n + 1 * 2)
  }
}