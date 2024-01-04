package bls
import scala.math.*

import scala.annotation.tailrec
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

extension (bigint: BigInt)
  def floorMod(q: BigInt): BigInt =
    val r = bigint % q
    if r < 0 then r + q else r

case class Fq private (q: BigInt, value: BigInt):

  def unary_- : Fq = Fq(q, -value)

  def +(other: Fq): Fq = Fq(q, value + other.value)

  def -(other: Fq): Fq = Fq(q, value - other.value)

  def *(other: Fq): Fq = Fq(q, value * other.value)

  def ==(other: Fq): Boolean =
    value == other.value && q == other.q

  def <(other: Fq): Boolean = value < other.value

  def >(other: Fq): Boolean = value > other.value

  def <=(other: Fq): Boolean = value <= other.value

  def >=(other: Fq): Boolean = value >= other.value

  def **(exp: BigInt): Fq = Fq.pow(q, value, exp)
  def unary_~ : Fq =
    @annotation.tailrec
    def loop(x0: BigInt, x1: BigInt, y0: BigInt, y1: BigInt, a: BigInt, b: BigInt): Fq =
      if a == 0 then Fq(this.q, x0)
      else
        val q  = b / a
        val b1 = a
        val a1 = b % a
        loop(x1, x0 - q * x1, y1, y0 - q * y1, a1, b1)
    loop(1, 0, 0, 1, q, value)

  def /(other: Fq): Fq  = this * ~other
  def /(other: Int): Fq = this * ~Fq(q, other)

  def modsqrt: Fq =
    if value == 0 then Fq(q, 0)
    else if value.modPow((q - 1) / 2, q) != 1 then
      throw new IllegalArgumentException("No sqrt exists")
    else if q % 4 == 3 then Fq(q, value.modPow((q + 1) / 4, q))
    else if q % 8 == 5 then Fq(q, value.modPow((q + 3) / 8, q))
    else
      // Tonelli Shanks algorithm for p % 8 == 1
      @annotation.tailrec
      def loop0(s: BigInt, q0: BigInt): (BigInt, BigInt) =
        if q0 % 2 == 0 then loop0(s + 1, q0 / 2)
        else (s, q0)
      val (s, q0) = loop0(0, q - 1)

      def loop1(z: BigInt, i: BigInt): BigInt =
        val euler = i.modPow((q - 1) / 2, q)
        if euler == BigInt(-1).mod(q) then i
        else if i >= q then 0
        else loop1(z, i + 1)
      val z = loop1(0, 0)

      @annotation.tailrec
      def loop2(m: BigInt, c: BigInt, t: BigInt, r: BigInt): Fq =
        if t == BigInt(0) then Fq(q, 0)
        else if t == BigInt(1) then Fq(q, r.toInt)
        else
          def loop3(i: Int, f: BigInt): Int =
            if f == BigInt(1) then i
            else loop3(i + 1, f.modPow(2, q))
          val i  = loop3(0, t)
          val b  = c.modPow(BigInt(2).modPow(m - i - 1, q), q)
          val m1 = i
          val c1 = b.modPow(2, q)
          val t1 = (t * c1).floorMod(q)
          val r1 = (r * b).floorMod(q)
          loop2(m1, c1, t1, r1)
      val t = value.modPow(q0, q)
      val r = value.modPow((q0 + 1) / 2, q)
      loop2(s, z.modPow(q0, q), t, r)

object Fq:
  def apply(q: BigInt, value: BigInt): Fq = new Fq(q, value.floorMod(q))
  def zero(q: BigInt): Fq              = Fq(q, 0)
  def one(q: BigInt): Fq               = Fq(q, 1)
  def fromFq(q: BigInt, fq: Fq): Fq    = fq

  @annotation.tailrec
  def pow(q: BigInt, value: BigInt, exp: BigInt, acc: BigInt = 1): Fq =
    exp.toInt match
      case 0 => Fq(q, acc)
      case 1 => Fq(q, value * acc)
      case _ if exp % 2 == 0 =>
        pow(q, (value * value).floorMod(q), exp / 2, acc)
      case _ =>
        pow(q, (value * value).floorMod(q), exp / 2, acc * value)

trait FieldExtBase[BaseField, Embedding <: Int]:
  def root: BaseField
  def extension: Int
  def q: BigInt
  def args: Seq[BaseField] :| Length[StrictEqual[Embedding]]

/*
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
 */

case class Fq2(q: BigInt, a: Fq, b: Fq) extends FieldExtBase[Fq, 2]:
  override def extension: Int = 2
  override def root: Fq = Fq(q, -1)
  override def args: Seq[Fq] :| Length[StrictEqual[2]] = Seq(a, b).assume

  def unary_- : Fq2 = Fq2(q, -a, -b)

  def +(other: Fq2): Fq2 = Fq2(q, a + other.a, b + other.b)

  def -(other: Fq2): Fq2 = Fq2(q, a - other.a, b - other.b)

  def *(other: Fq2): Fq2 =
    val values = for
      (x, i) <- Seq(a, b).zipWithIndex
      (y, j) <- Seq(other.a, other.b).zipWithIndex
    yield
      val value = if i + j >= valueOf[2] then x * y * root else x * y
      (Math.floorMod(i + j, valueOf[2]), value)
    val valueMap = values.groupMapReduce(_._1)(_._2)(_ + _)
    Fq2(q, valueMap(0), valueMap(1))

  def **(e: Int :| Positive): Fq2 =
    def loop(e: Int, ans: Fq2, base: Fq2): Fq2 =
      if e == 0 then ans else
        val ans1 = if (e & 1) == 1 then ans * base else ans
        val base1 = base * base
        val e1 = e >> 1
        loop(e1, ans1, base1)

    loop(e, Fq2.one(q), this)

  def pow(e: Int :| Positive): Fq2 = this ** e

  def modsqrt: Fq2 =
    if b == Fq.zero(q) then Fq2(q, a.modsqrt, b)
    else
      val alpha = a ** 2 + b ** 2
      val gamma = alpha ** ((q - 1) / 2)
      if gamma == Fq(q, -1) then
        throw new IllegalArgumentException("No sqrt exists")
      else
        val alpha1 = alpha.modsqrt
        val delta = (a + alpha1) * ~Fq(q, 2)
        val gamma1 = delta ** ((q - 1) / 2)
        val delta1 =
          if gamma1 == Fq(q, -1) then (a - alpha1) * ~Fq(q, 2)
          else delta
        val a1 = delta1.modsqrt
        val b1 = b * ~(Fq(q, 2) * a1)
        Fq2(q, a1, b1)


object Fq2:
  def one(q: BigInt): Fq2 = Fq2(q, Fq.one(q), Fq.zero(q))

/*
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

 */
