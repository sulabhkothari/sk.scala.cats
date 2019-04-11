object FoldableAndTraverse {
  def main(args: Array[String]): Unit = {
    reflectingOnFolds

    val s = SList(List(23, 7, 7, 90, 6, 44))
    println(s.map(_ * 2))
    println(s.flatMap(x => List(x * 2, x * 3, x * 4)))
    println(s.filter(_ % 2 == 0))
    println(s.sum(_ + _))
    import cats.instances.int._
    println(s.sumWithMonoid)
    println(sumWithNumeric(s.list))

    foldableInCats

    foldingWithMonoids
    import cats.Eval
    println("Stack safe list fold --> " + List(1, 2, 3, 4, 5, 6).foldRight(Eval.now(0))((i, a) => a.map(_ + i)).value)

    traverse
  }

  import scala.math.Numeric

  def sumWithNumeric[A](list: List[A])
                       (implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)

  def reflectingOnFolds = {
    val l = List(12, 3, 4, 99, 8, 7)
    println(l.foldLeft(List.empty[Int])((acc, item) => item :: acc))
    println(l.foldRight(List.empty[Int])((item, acc) => item :: acc))
  }

  import scala.collection.immutable

  class Default[+A](val default: A)

  trait LowerPriorityImplicits {
    // Stop AnyRefs from clashing with AnyVals
    implicit def defaultNull[A <: AnyRef]: Default[A] = new Default[A](null.asInstanceOf[A])
  }

  object Default extends LowerPriorityImplicits {

    implicit object DefaultDouble extends Default[Double](0.0)

    implicit object DefaultFloat extends Default[Float](0.0F)

    implicit object DefaultInt extends Default[Int](0)

    implicit object DefaultLong extends Default[Long](0L)

    implicit object DefaultShort extends Default[Short](0)

    implicit object DefaultByte extends Default[Byte](0)

    implicit object DefaultChar extends Default[Char]('\u0000')

    implicit object DefaultBoolean extends Default[Boolean](false)

    implicit object DefaultUnit extends Default[Unit](())

    implicit def defaultSeq[A]: Default[immutable.Seq[A]] = new Default[immutable.Seq[A]](immutable.Seq())

    implicit def defaultSet[A]: Default[Set[A]] = new Default[Set[A]](Set())

    implicit def defaultMap[A, B]: Default[Map[A, B]] = new Default[Map[A, B]](Map[A, B]())

    implicit def defaultOption[A]: Default[Option[A]] = new Default[Option[A]](None)

    def value[A](implicit value: Default[A]): A = value.default
  }

  case class SList[T](list: List[T]) {
    def map[U](f: T => U): SList[U] = {
      SList[U](list.foldRight(List.empty[U])((item, acc) => f(item) :: acc))
    }

    def flatMap[U](f: T => List[U]): SList[U] = {
      SList[U](list.foldRight(List.empty[U])((item, acc) => f(item).foldRight(acc)((i, a) => i :: a)))
    }

    def filter(predicate: T => Boolean) =
      SList(list.foldRight(List.empty[T])((i, a) => if (predicate(i)) i :: a else a))

    def sum(implicit summer: (T, T) => T) = list.foldRight(Default.value[T])((i, s) => summer(i, s))

    import cats.Monoid

    def sumWithMonoid
    (implicit monoid: Monoid[T]): T =
      list.foldRight(monoid.empty)(monoid.combine)
  }

  def foldableInCats = {
    // We can summon instances as usual using Foldable.apply and call their implementations of foldLeft directly.
    // Here is an example using List:
    import cats.Foldable
    import cats.instances.list._ // for Foldable
    val ints = List(1, 2, 3)
    println("Foldable for list --> " + Foldable[List].foldLeft(ints, 0)(_ + _))

    import cats.instances.option._ // for Foldable
    val maybeInt = Option(123)
    println("Foldable for list --> " + Foldable[Option].foldLeft(maybeInt, 10)(_ * _))
    // Foldable defines foldRight differently to foldLeft, in terms of the Eval monad:
    // def foldRight[A, B](fa: F[A], lb: Eval[B])
    //                     (f: (A, Eval[B]) => Eval[B]): Eval[B]

    //Using Eval means folding is always stack safe, even when the collection’s default defini􏰀on of foldRight is not.
    // For example, the default implementation of foldRight for Stream is not stack safe. The longer the stream,
    // the larger the stack requirements for the fold. A sufficiently large stream will trigger a StackOverflowError:
    def stackOverflow = {
      import cats.Eval
      import cats.Foldable
      def bigData = (1 to 100000).toStream

      bigData.foldRight(0L)(_ + _)
    }
    // stackOverflow

    import cats.Eval
    import cats.instances.stream._ // for Foldable
    def bigData = (1 to 100000).toStream

    val eval: Eval[Long] =
      Foldable[Stream].
        foldRight(bigData, Eval.now(0L)) { (num, eval) =>
          eval.map(_ + num)
        }
    println("Foldable stack safe foldRight --> " + eval.value)

    // Stack safety isn’t typically an issue when using the standard library. The most commonly used collec􏰀on types,
    // such as List and Vector, provide stack safe implementations of foldRight. We’ve called out Stream because it is
    // an exception to this rule.
  }

  def foldingWithMonoids = {
    import cats.Foldable
    import cats.instances.option._
    import cats.instances.list._
    // Foldable provides us with a host of useful methods defined on top of foldLeft. Many of these are facsimiles of
    // familiar methods from the standard library: find, exists, forall, toList, isEmpty, nonEmpty, and so on:
    println("Folding with Monoids (Option) --> " + Foldable[Option].nonEmpty(Option(42)))
    println("Folding with Monoids (List) --> " + Foldable[List].find(List(1, 2, 3))(_ % 2 == 0))

    import cats.instances.int._ // for Monoid
    println("Folding with Monoids (combineAll) --> " + Foldable[List].combineAll(List(1, 2, 3)))

    import cats.instances.string._ // for Monoid
    println("FoldMap (to convert each Int to a String and con- catenate them) --> " + Foldable[List].foldMap(List(1, 2, 3))(_.toString))

    // we can compose Foldables to support deep traversal of nested sequences:
    import cats.instances.vector._ // for Monoid
    val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
    println("Folding with Monoids (deep traversal of nested sequences) --> "
      + (Foldable[List] compose Foldable[Vector]).combineAll(ints))

    import cats.syntax.foldable._ // for combineAll and foldMap List(1, 2, 3).combineAll
    // res16: Int = 6
    println("Syntax for Foldable --> " + List(1, 2, 3).foldMap(_.toString))

    // the following generic code will use Foldable:
    import scala.language.higherKinds
    def sum[F[_] : Foldable](values: F[Int]): Int =
      values.foldLeft(0)(_ + _)

    println("Force to use foldable --> " + sum(List(9, 89, 77)))
  }

  def traverse = {
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    val hostnames = List(
      "alpha.example.com",
      "beta.example.com",
      "gamma.demo.com"
    )

    def getUptime(hostname: String): Future[Int] =
      Future(hostname.length * 60) // just for demonstration

    val allUptimes: Future[List[Int]] = hostnames.foldLeft(Future(List.empty[Int])) {
      (accum, host) =>
        val uptime = getUptime(host)
        for {
          accum <- accum
          uptime <- uptime
        } yield accum :+ uptime
    }

    // foldLeft and foldRight are flexible iteration methods but they require us to do a lot of work to define
    // accumulators and combinator func􏰀ons. The Traverse type class is a higher level tool that leverages Applicatives
    // to provide a more convenient, more lawful, pa􏰂ern for iteration.

    def traversingWithFutures = {
      // We can demonstrate Traverse using the Future.traverse and Future.sequence methods in the Scala standard
      // library. These methods provide Future-specific implementations of the traverse patt􏰂ern. As an example, suppose
      // we have a list of server hostnames and a method to poll a host for its uptime:

      println("Combining futures the hard way --> " + Await.result(allUptimes, 1.second))

      // Intuitively, we iterate over hostnames, call func for each item, and combine the results into a list. This
      // sounds simple, but the code is fairly unwieldy be- cause of the need to create and combine Futures at every
      // iteration. We can improve on things greatly using Future.traverse, which is tailor-made for this patt􏰂ern:
      val allUptimes2: Future[List[Int]] =
      Future.traverse(hostnames)(getUptime)
      println("Combining futures using traverse --> " + Await.result(allUptimes2, 1.second))

      // This is essentially the same as our example code above. Future.traverse is abstracting away the pain of folding
      // and defining accumulators and combination functions. It gives us a clean high-level interface to do what we
      // want:
      //• start with a List[A];
      //• provide a function A=>Future[B];
      //• endup with a Future[List[B]].

      // The standard library also provides another method, Future.sequence, that assumes we’re starting with a
      // List[Future[B]] and don’t need to provide an identity function:
      //object Future {
      //def sequence[B](futures: List[Future[B]]): Future[List[B]] =
      //    traverse(futures)(identity)
      //// etc...
      //}

      // In this case the intuitive understanding is even simpler:
      //• start with a List[Future[A]];
      //• end up with a Future[List[A]].

      // Future.traverse and Future.sequence solve a very specific problem: they allow us to iterate over a sequence of
      // Futures and accumulate a result.

      // The simplified examples above only work with Lists, but the real Future.traverse and Future.sequence work
      // with any standard Scala collection.
    }

    def traversingWithApplicatives = {
      // Future(List.empty[Int]) is equivalent to Applicative.pure:

      import cats.instances.future._ // for Applicative
      import cats.syntax.applicative._ // for pure
      import scala.concurrent._
      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.syntax.apply._

      List.empty[Int].pure[Future]

      def newCombine(accum: Future[List[Int]],
                     host: String): Future[List[Int]] =
        (accum, getUptime(host)).mapN(_ :+ _)

      // By substituting these snippets back into the definition of traverse we can generalise it to to work with any Applicative:
      import scala.language.higherKinds
      import cats.Applicative

      // :+ is part of SeqLike and List is a sequence
      // Start with F[List[B]] (List.empty[B].pure[F])
      // Actual in listSequence is List[F[B]] so identity returns F[B] with foldLeft
      // Use Semigroupal on F to join values inside F i.e., F[List[B]] and F[B] to get F[List[B] :+ B]

      def listTraverse[F[_] : Applicative, A, B]
      (list: List[A])(func: A => F[B]): F[List[B]] =
        list.foldLeft(List.empty[B].pure[F]) { (accum, item) => (accum, func(item)).mapN(_ :+ _)
        }

      // A can also be F[B] so that identity returns F[B] itself
      def listSequence[F[_] : Applicative, B]
      (list: List[F[B]]): F[List[B]] =
        listTraverse(list)(identity)

      import cats.instances.option._
      import cats.syntax.apply._

      // F is Option in this example, :+ is part of SeqLike and List is a sequence
      println("Pure value --> " + (List.empty[Int].pure[Option]))
      println("Identity value --> " + (List.empty[Int].pure[Option], identity(Option(12))).mapN(_ :+ _))
      println("Semigroupal of option --> " + (List(10).pure[Option], Some(89)).mapN((a, i) => a :+ i))

      val totalUptime = listTraverse(hostnames)(getUptime)
      println("Traverse generalised to work with Applicatives --> " + Await.result(totalUptime, 1.second))
    }

    def excerciseTraversingWithVectorsAndOptions = {
      import cats.Applicative
      import cats.syntax.apply._
      import cats.syntax.applicative._ // for pure
      def listTraverse[F[_] : Applicative, A, B]
      (list: List[A])(func: A => F[B]): F[List[B]] =
        list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
          println(s"A = $accum, I = $item, f = ${func(item)}")
          (accum, func(item)).mapN(_ :+ _)
        }

      // A can also be F[B] so that identity returns F[B] itself
      def listSequence[F[_] : Applicative, B]
      (list: List[F[B]]): F[List[B]] =
        listTraverse(list)(identity)
      import cats.instances.vector._ // for Applicative
      println("Excercise (Traversing with Vectors)1 --> " + listSequence(List(Vector(1, 2), Vector(3, 4))))

      println("Semigroupal[Vector].product implemented in terms of flatMap (cartesian product) --> "
        + (List(3, 4).pure[Vector], Vector(12, 23)).mapN(_ :+ _))

      println("Excercise (Traversing with Vectors)2 --> " + listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))

      import cats.instances.option._ // for Applicative
      def process(inputs: List[Int]) =
        listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

      def getOnlyIfEven(n: Int) =
        if (n % 2 == 0) Some(n) else None

      // Option is a monad, so the semigroupal combine func􏰀on follows from flatMap. The seman􏰀cs are therefore
      // fail-fast error handling: if all inputs are even, we get a list of outputs. Otherwise we get None
      println("Excercise (Traversing with Options)1 --> " + process(List(2, 4, 6)))
      println("Excercise (Traversing with Options)2 --> " + process(List(2, 1, 3)))
      // Throws error without getOnlyIfEven i.e., if None is directly passed
      println("Semigroupal Apply syntax with Options --> " + (List(90).pure[Option], getOnlyIfEven(99)).mapN(_ :+ _))

      import cats.data.Validated
      import cats.instances.list._ // for Monoid
      type ErrorsOr[A] = Validated[List[String], A]

      def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] = listTraverse(inputs) { n =>
        if (n % 2 == 0) {
          Validated.valid(n)
        } else {
          Validated.invalid(List(s"$n is not even"))
        }
      }

      println("Excercise (Traversing with Validated)1 --> " + processValidated(List(2, 4, 6)))
      println("Excercise (Traversing with Validated)2 --> " + processValidated(List(1, 2, 3)))
    }

    def traverseWithCats = {
      // Our listTraverse and listSequence methods work with any type of Applicative, but they only work with one type
      // of sequence: List. We can generalise over different sequence types using a type class, which brings us to Cats’
      // Traverse. Here’s the abbreviated definition:
      //      package cats
      //      trait Traverse[F[_]] {
      //        def traverse[G[_]: Applicative, A, B]
      //          (inputs: F[A])(func: A => G[B]): G[F[B]]
      //        def sequence[G[_]: Applicative, B]
      //          (inputs: F[G[B]]): G[F[B]] = traverse(inputs)(identity)
      //      }

      // Cats provides instances of Traverse for List, Vector, Stream, Option, Either, and a variety of other types.
      // We can summon instances as usual using Traverse.apply and use the traverse and sequence methods as described
      // in the previous section:
      import cats.Traverse
      import cats.instances.future._ // for Applicative
      import cats.instances.list._ // for Traverse
      val totalUptime: Future[List[Int]] = Traverse[List].traverse(hostnames)(getUptime)
      println("Traverse in Cats (hostnames-getUptime calls) --> " + Await.result(totalUptime, 1.second))
      val numbers: List[Future[Int]] = List(Future(1), Future(2), Future(3))
      val numbers2 = Traverse[List].sequence(numbers)
      println("Traverse in Cats (Sequence multiple Future[Int]) --> " + Await.result(numbers2, 1.second))

      // There are also syntax versions of the methods, imported via cats.syntax.traverse:
      import cats.syntax.traverse._ // for sequence and traverse

      println("Traverse in Cats syntax-version (hostnames-getUptime calls) --> " + Await.result(hostnames.traverse(getUptime), 1.second))
      println("Traverse in Cats syntax-version (Sequence multiple Future[Int]) --> " + Await.result(numbers.sequence[Future,Int], 1.second))
    }

    traversingWithFutures
    traversingWithApplicatives
    excerciseTraversingWithVectorsAndOptions
    traverseWithCats
  }
}
