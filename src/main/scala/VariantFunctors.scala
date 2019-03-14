object ContravariantFunctor {

  def main(args: Array[String]): Unit = {
    println(format("hello"))
    println(format(true))
    println(Box(90).vle)
    println(format(Box(true)))
  }

  // Contravariant Functor
  trait Printable[A] {
    self =>
    def format(value: A): String

    // Contramap method
    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String =
          self.format(func(value))
      }

    //The contramap method only makes sense for data types that represent transformati􏰂ons
    //For example, we can’t define contramap for an Option because there is no way of
    // feeding a value in an Option[B] backwards through a function A => B
  }


  //Single Abstract Method Implementation
  implicit val stringPrintable: Printable[String] =
    (value: String) => "\"" + value + "\""

  //Trait Implementation
  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if (value) "yes" else "no"
  }

  final case class Box[A](vle: A)

  //  implicit def boxPrintable[A](implicit p: Printable[A]) = new Printable[Box[A]] {
  //    def format(box: Box[A]): String =
  //      p.format(box.vle)
  //  }

  // Strange: This does not work
  //implicit def boxPrintable[A](implicit p: Printable[A]) = p.contramap((x: Box[A]) => x.vle)
  // This Works
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = p.contramap(_.vle)

  def format[A](value: A)(implicit p: Printable[A]) = p.format(value)
}

object InvariantFunctor {
  // Invariant functors implement a method called imap that is informally equivalent to a combination of map and contramap.
  // If map generates new type class instances by appending a function to a chain, and contramap generates them by prepending
  // an operation to a chain, imap generates them via a pair of bidirectional transformations.

  def main(args: Array[String]): Unit = {
    println(encode(123.4))
    println(decode[Double]("123.4"))
    println(encode(Box(123.4)))
    println(decode[Box[Double]]("123.4"))
  }

  trait Codec[A] {
    self =>
    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value

      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  case class Box[A](value: A)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap(Box(_), _.value)

}

object CatsVariantFunctors {
  def main(args: Array[String]): Unit = {
    import cats.Contravariant
    import cats.Show
    import cats.instances.string._
    //import cats.syntax.show._

    val showString = Show[String]
    val showSymbol = Contravariant[Show].contramap(showString)((sym:Symbol) => s"""'${sym.name}""")

    println(showSymbol.show('sk))

    import cats.syntax.contravariant._ // for contramap
    println(sho]
    wString.contramap[Symbol](x=>s"''${x.name}''").show('dave))

    import cats.Invariant
    import cats.Monoid
    val monoidString = Monoid[String]
    implicit val monoidSymbol = Invariant[Monoid].imap(monoidString)(Symbol(_))(_.name)
    import cats.syntax.semigroup._
    println('legendary |+| 'weapon |+| 'here)
  }
}