

import scala.concurrent.Future

object MonadTransformers {

  def main(args: Array[String]): Unit = {
    println("Hello Transformers")
    transformer
    monadStacks
    monadEitherT
    constructingAndUnpackingInstances
  }

  //  import cats.Monad
  //  import cats.syntax.applicative._ // for pure
  //  import cats.syntax.flatMap._ // for flatMap
  //  import scala.language.higherKinds
  // Hypothetical example. This won't actually compile:
  //  def compose[M1[_] : Monad, M2[_] : Monad] = {
  //    type Composed[A] = M1[M2[A]]
  //    new Monad[Composed] {
  //      def pure[A](a: A): Composed[A] =
  //        a.pure[M2].pure[M1]
  //
  //      def flatMap[A, B](fa: Composed[A])
  //                       (f: A => Composed[B]): Composed[B] =
  //      // Problem! How do we write flatMap?
  //        implicitly[Monad[M1]].flatMap(fa)(x => implicitly[Monad[M2]].flatMap(x)(y => {
  //          //val m: M2[B] = _
  //          (new AnyRef()).asInstanceOf[M2[B]]
  //        }))
  //    }
  //  }

  def transformer = {
    import cats.data.OptionT
    type ListOption[A] = OptionT[List, A]
    import cats.Monad
    import cats.instances.list._ // for Monad
    import cats.syntax.applicative._ // for pure
    val result1: ListOption[Int] = OptionT(List(Option(10))) // result1: ListOption[Int] = OptionT(List(Some(10)))
    val result2: ListOption[Int] = 32.pure[ListOption] // result2: ListOption[Int] = OptionT(List(Some(32)))
    val result3 = result1.flatMap { (x: Int) =>
      result2.map { (y: Int) =>
        x + y
      }
    }

    println(result3)
  }

  //case class EitherT[F[_], E, A](stack: F[Either[E, A]])

  def monadStacks = {
    import cats.data.OptionT
    import scala.util.{Try, Success, Failure}
    import cats.syntax.applicative._

    type ErrorOr[A] = Either[String, A]
    // Build our final monad stack using OptionT:
    type ErrorOrOption[A] = OptionT[ErrorOr, A]

    import cats.instances.either._ // for Monad
    val a = 10.pure[ErrorOrOption]
    // a: ErrorOrOption[Int] = OptionT(Right(Some(10)))
    val b = 0.pure[ErrorOrOption]
    val c = a.flatMap(x => b.map(y => Try(x / y) match {
      case Success(r) => Right(r)
      case Failure(f) => Left(f)
    }))
    val result = for {
      x <- a
      y <- b
    }
      yield x + y

    println("Option[Either[String,A]] ==> " + c.value.fold(x => x, x => x.get))
    println("Option[Either[String,A]] ==> " + result.value.fold(x => x, x => x.get))
  }

  def monadEitherT = {
    import cats.data.EitherT
    import cats.instances.future._ // for Monad
    import scala.concurrent.Await
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    import cats.syntax.applicative._
    import cats.data.OptionT
    type EitherFuture[A] = EitherT[Future, String, A]
    type FutureEitherOption[A] = OptionT[EitherFuture, A]
    val futureEitherOr: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption]
        b <- 32.pure[FutureEitherOption]
      } yield a + b
    futureEitherOr.value.value.onComplete {
      case scala.util.Success(value) => println("+++++++===>" + value)
      case _ =>
    }

    10.pure[FutureEitherOption].flatMap(a => 32.pure[FutureEitherOption].map(b => a + b))
    val intermediate = futureEitherOr.value
    val stack = intermediate.value
    println("from await --> " + Await.result(stack, 1.second))

    // Kind Projector
    //If you frequently find yourself defining mul􏰀ple type aliases when building monad stacks, you may want to try the
    // Kind Projector compiler plugin. Kind Projector enhances Scala’s type syntax to make it easier to define par􏰀tially
    // applied type constructors.
    // import cats.instances.option._ // for Monad
    // 123.pure[EitherT[Option, String, ?]]
  }

  def constructingAndUnpackingInstances = {
    import cats.data.{OptionT}
    import cats.instances.either._ // for Monad
    import cats.syntax.applicative._
    type ErrorOr[A] = Either[String, A]
    type ErrorOrOption[A] = OptionT[ErrorOr, A]
    //val errorStack1 = OptionT(Right(Some(10))) //Doesn't work
    val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
    println("Unpacking Errorstack1==> " + errorStack1.value)

    val errorStack2 = 32.pure[ErrorOrOption]
    println("Unpacking Errorstack2==> " + errorStack2.value)

    // Once we’ve finished with a monad transformer stack, we can unpack it using its value method. This returns the
    // untransformed stack. We can then manipulate the individual monads in the usual way:
    println(errorStack1.value)
    println(errorStack2.value.map(_.getOrElse(-1)))
  }

  // Default Instances
  //Many monads in Cats are defined using the corresponding transformer and the Id monad. This is reassuring as it
  // confirms that the APIs for monads and transformers are identical. Reader, Writer, and State are all defined in this
  //way:
  //In other cases monad transformers are defined separately to their corresponding monads. In these cases, the methods
  // of the transformer tend to mirror the methods on the monad. For example, OptionT defines getOrElse, and EitherT
  // defines fold, bimap, swap, and other useful methods.
}

object UsagePatterns {
  // One approach involves crea􏰀ng a single “super stack” and sticking to it throughout our code base. This works if
  // the code is simple and largely uniform in nature. For example, in a web application, we could decide that all
  // request handlers are asynchronous and all can fail with the same set of HTTP error codes. We could design a custom
  // ADT representing the errors and use a fusion Future and Either everywhere in our code:
  import cats.data.EitherT

  sealed abstract class HttpError

  final case class NotFound(item: String) extends HttpError

  final case class BadRequest(msg: String) extends HttpError // etc...
  type FutureEither[A] = EitherT[Future, HttpError, A]


  // The “super stack” approach starts to fail in larger, more heterogeneous code bases where different stacks make
  // sense in different contexts. Another design pa􏰁ern that makes more sense in these contexts uses monad transformers
  // as local “glue code”. We expose untransformed stacks at module boundaries, transform them to operate on them
  // locally, and untransform them before passing them on. This allows each module of code to make its own decisions
  // about which transformers to use:
  import cats.data.Writer
  import cats.instances.either._ // for Monad
  import cats.syntax.applicative._

  type Logged[A] = Writer[List[String], A]

  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] = util.Try(str.toInt).toOption match {
    case Some(num) => Writer(List(s"Read $str"), Some(num))
    case None => Writer(List(s"Failed on $str"), None)
  }

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    import cats.instances.list._
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }

  def main(args: Array[String]): Unit = {
    // This approach doesn't force OptionT on other users' code:
    val result1 = addAll("1", "2", "3")
    val result2 = addAll("1", "a", "3")
    println("RESULT1 -> " + result1)
    println("RESULT2 -> " + result2)
  }
}

object ExcerciseOnTransformers {

  import cats.data.EitherT
  import cats.instances.future._ // for Monad
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import cats.syntax.applicative._
  import cats.data.OptionT

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(power) => EitherT.right(Future(power))
    case None => EitherT.left(Future("Unreachable"))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    power1 <- getPowerLevel(ally1)
    power2 <- getPowerLevel(ally2)
  } yield (power1 + power2) > 15

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value
    Await.result(stack, 1.second) match {
      case Left(msg) =>
        s"Comms error: $msg"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }

  def main(args: Array[String]): Unit = {
    println(Await.result(getPowerLevel("jk").value, 1.second))
    println(Await.result(getPowerLevel("Bumblebee").value, 1.second))
    println(tacticalReport("Bumblebee","Jazz"))
    println(tacticalReport("Bumblebee","Hot Rod"))
    println(tacticalReport("Jazz", "Ironhide"))
  }
}
