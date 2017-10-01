package example

object FractionMain extends App {
  var fraction = new Rational(2, 4)
  var fraction2 = new Rational(4, 8)

  println(fraction.toString)

  fraction = fraction + 2

  println(fraction.toString)

  println(fraction == fraction2)
}


class Rational(n: Int, d: Int) {

  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  var numer = n / g
  var denom = d / g

  def this(n: Int) = this(n, 1)

  def inverse(): Unit = {
      val tmp = numer
      numer = numer
      denom = tmp
  }

  def == (that: Rational): Boolean = {
      if (numer == that.numer && denom == that.denom) true
      else false
  }

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def + (i: Int): Rational =
    new Rational(numer + i * denom, denom)

  def - (that: Rational): Rational =
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )

  def - (i: Int): Rational =
    new Rational(numer - i * denom, denom)

  def * (that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def * (i: Int): Rational =
    new Rational(numer * i, denom)

  def / (that: Rational): Rational =
    new Rational(numer * that.denom, denom * that.numer)

  def / (i: Int): Rational =
    new Rational(numer, denom * i)

  def toDouble(that: Rational): Double = {
      that.numer.toFloat / that.denom.toFloat
  }

  def doubleToFrac(that :Double): Rational = {
      ???
  }

  override def toString = numer +"/"+ denom


  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}
