
  val r1 = new Rational(1,3)
  r1.numer
  r1.denom

  val r2 = new Rational(5,7)
  r1.add(r2)
  r1.sub(r2)

  val r3 = new Rational(3,2)

  r1.sub(r2).sub(r3)

  r2.neg


class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) = {
    new Rational(
      ( that.numer * denom + numer * that.denom ),
      (denom * that.denom)
    )
  }

  def neg() = new Rational(-numer,denom)

  def sub(that: Rational) = add(that.neg)

  override def toString(): String = {
    numer + "/" + denom
  }

}