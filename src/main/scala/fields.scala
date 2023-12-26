package bls
import scala.math._
import scala.Predef._
import scala.collection.mutable._
import org.web3j.utils.Numeric

import scala.annotation.tailrec

class Fq(val Q: Int, private val _value: Int) {
  val value: Int = _value % Q

  def unary_- : Fq = new Fq(Q, -value)

  def +(other: Fq): Fq = new Fq(Q, value + other.value)

  def -(other: Fq): Fq = new Fq(Q, value - other.value)

  def *(other: Fq): Fq = new Fq(Q, value * other.value)

  def /(other: Fq): Fq = this * other.inverse

  def **(exp: Int): Fq = {
    @tailrec
    def powHelper(base: Fq, exponent: Int, result: Fq): Fq = exponent match {
      case 0 => result
      case _ if exponent % 2 == 0 => powHelper(base * base, exponent / 2, result)
      case _ => powHelper(base * base, exponent / 2, result * base)
    }

    powHelper(this, exp, new Fq(Q, 1))
  }

  def inverse: Fq = {
    @tailrec
    def gcdExtended(a: Int, b: Int, x: Int, y: Int): (Int, Int) = {
      if (a == 0) (y, b)
      else {
        val (newY, newB) = gcdExtended(b % a, a, y - (b / a) * x, x)
        (newY, newB)
      }
    }

    val (inv, _) = gcdExtended(value, Q, 1, 0)
    new Fq(Q, inv)
  }

  override def equals(other: Any): Boolean = other match {
    case that: Fq => value == that.value && Q == that.Q
    case _ => false
  }

  override def hashCode(): Int = (value, Q).##

  override def toString: String = s"Fq(${value.toHexString})"
}

trait FieldExtBase[A] {
    def q: Int
    def root: A
}

object FieldExtBase:
  extension [A <: FieldExtBase[_]](a: A)
    def pow(n: Int): A = ???
    // def -(other: A): A = subs(a, other)

  extension (a: Fq2)
    def +(other: Fq): Fq2 = add(a, other)
    def *(other: Fq2): Fq2 = mul(a, other)
    def pow(e: Int): Fq2 = power(a, e)

  def zero(q: Int): Fq2 = fromFq(q, Fq(q, 0))

  def fromFq(q: Int, fq: Fq): Fq2 =
    val y = fq
    val z = Fq(q, 0)
    Fq2(Seq(y, z), q, Fq(q, -1))

//   def subs(other: Any): FieldExtBase = ???

  def add(self: Fq2, other: Fq): Fq2 =
    val otherValue = self.value.map(_ => Fq.zero(self.q)).updated(0, other)

    val retValue = self.value.zip(otherValue).map{ case (a, b) => a + b}

    Fq2(retValue, self.q, self.root)


  def mul(a: Fq2, b: Fq2): Fq2 =
    val buf: Seq[Fq] = a.value.map(_ => Fq.zero(a.q))
    val pairs = for
      (x, i) <- a.value.zipWithIndex
      (y, j) <- b.value.zipWithIndex
    yield (i, x, j, y)

    val buf1: Seq[Fq] = pairs.foldLeft(buf){ case (buf, (i, x, j, y)) =>
      val index = (i + j) % 2
      if x.value > 0 && y.value > 0 then
        buf.updated(index, buf(index) + x * y * Fq(a.q, -1))
      else
        buf.updated(index, buf(index) + x * y)
    }
    Fq2(buf1, a.q, a.root)
  
  def power(e: Int): Fq = {
    assert(e >= 0)
    var ans = this.one(this.Q)
    var base = this
    ans.root = this.root

    var ee = e
    while (ee != 0) {
        if ((ee & 1) == 1) {
        ans *= base
        }

        base *= base
        ee >>= 1
    }

    ans
    }


// //   def mul_by_nonresidue: Fq2 =
// //     val (a, b) = this
// //     Fq2(Q, a - b, a + b)


case class Fq2(value: Seq[Fq], q: Int, root: Fq) extends FieldExtBase[Fq]:

  def modsqrt: Fq2 =
    val Seq(a0: Fq, a1: Fq) = value
    if a1 == Fq.zero(q) then a0.modsqrt
    val alpha = pow(a0, 2) + pow(a1, 2)
    val gamma = pow(alpha, (q - 1) / 2)
    if gamma == Fq(q, -1) then
      throw new ValueError("No sqrt exists")
    val alpha1 = alpha.modsqrt
    val delta = (a0 + alpha1) * ~Fq(Q, 2)
    val gamma1 = pow(delta, (Q - 1) / 2)
    if gamma1 == Fq(Q, -1) then
      val delta = (a0 - alpha1) * Fq(Q, 2).neg
    val x0 = delta.modsqrt
    val x1 = a1 * (Fq(Q, 2) * x0)
    Fq2(Q, x0, x1)

   

    //    def modsqrt: Fq2 = ???
// //     val (a0, a1) = this
// //     if a1 == Fq.zero(Q):
// //       return a0.modsqrt
// //     val alpha = pow(a0, 2) + pow(a1, 2)
// //     val gamma = pow(alpha, (Q - 1) / 2)
// //     if gamma == Fq(Q, -1):
// //       throw new ValueError("No sqrt exists")
// //     val alpha = alpha.modsqrt
// //     val delta = (a0 + alpha) * ~Fq(Q, 2)
// //     val gamma = pow(delta, (Q - 1) / 2)
// //     if gamma == Fq(Q, -1):
// //       delta = (a0 - alpha) * ~Fq(Q, 2)
// //     val x0 = delta.modsqrt
// //     val x1 = a1 * ~(Fq(Q, 2) * x0)
// //     Fq2(Q, x0, x1)
end Fq2

object Fq2:
  def apply(q: Int, a: Fq, b: Fq): Fq2 = Fq2(Seq(a, b), q, Fq(q, -1))

