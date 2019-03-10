object VarianceModelling {
  sealed trait F{
    def print
  }

  class Sf() extends F{
    override def print: Unit = println("Sf")
  }

  final class CSf() extends Sf

  trait FI[-T]{
    def print(t:T) = println(t)
  }

  def ex(c:CSf)(implicit f: FI[CSf]) = f.print(c)

  def main(args: Array[String]): Unit = {
    implicit val f1 = new FI[Sf] {}

    ex(new CSf())
  }
}
