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

  }

  trait Codec[A] {
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = ???
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

  case class Box[A](value: A)
}