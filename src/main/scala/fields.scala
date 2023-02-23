package bls
import scala.math._
import scala.Predef._
import scala.collection.mutable._
import org.web3j.utils.Numeric


class Fq(val Q: Int, val value: BigInt):
    val extension: Int = 1

    def neg : Fq = new Fq(Q, -value)

    def +(other: Fq): Fq =
        if !other.isInstanceOf[Fq] then
            throw new Exception("not implemented")
        new Fq(Q, value + other.value)

    def -(other: Fq): Fq =
        if !other.isInstanceOf[Fq] then
            throw new Exception("not implemented")
        new Fq(Q, other.value - value)
    
    def *(other: Fq): Fq = Fq(Q, value * other.value)

    def /(other: BigInt | Fq): Fq =
        if (other.isInstanceOf[Int] && !other.isInstanceOf[Fq]) then
            Fq(Q, other.toBigInt)
        else
            other.neg

    override def equals(other: Any): Boolean = {
        if other.isInstanceOf[Fq] then
            val otherFq = other.asInstanceOf[Fq]
            this.value == otherFq.value && this.Q == otherFq.Q
        false
    }

    def <(other: Fq): Boolean = this.value < other.value

    def >(other: Fq): Boolean = this.value > other.value

    def <=(other: Fq): Boolean = this.value <= other.value

    def >=(other: Fq): Boolean = this.value >= other.value

    override def toString: String = {
        val s = Numeric.toHexString(this.value.toByteArray)
        val s2 = if s.length > 10 then
            s.slice(0, 7) + ".." + s.slice(-5, s.length()) 
            else
                s
        "Fq(" + s2 + ")"}

    def __repr__ = "Fq(" + Numeric.toHexString(this.value.toByteArray) + ")"

    def toByteArray: Array[Byte] = this.value.toByteArray

    def fromBytes(buffer: Array[Byte], q: Int): Fq =
        require(buffer.length == 48)
        new Fq(q, BigInt(buffer))

    def pow(other: Fq): Fq = 
        if (other.Q == 0) then
            Fq(Q, 1)
        else if (other.Q == 1) then
            Fq(Q, value)
        else if (other.Q % 2 == 0) then

            (Fq(Q, value * value), pow(other / 2))
        else 
            (Fq(Q, value * value), pow(other / 2)) * this


    def qiPower(i: Int): Fq = this

    def invert: Fq =
        var x0 = 1
        var x1 = 0
        var y0 = 0
        var y1 = 1
        var a = Q.toInt
        var b = value.toInt
        while (a != 0) {
            val q = b / a
            val (a1, b1) = (a, b % a)
            b = a
            a = a1
            val (x10, x11) = (x1, x0 - q * x1)
            x0 = x1
            x1 = x10
            val (y10, y11) = (y1, y0 - q * y1)
            y0 = y1
            y1 = y10
        }
        Fq(Q, x0)
    def floor(other: Any): Fq =
        other match
            case i: Int if !other.isInstanceOf[Fq] => this * Fq(Q, i).neg
            case o: Fq => this * o.neg
            case _ => throw new Exception("Invalid argument type")
    
    def iterator: Iterator[Fq] = Iterator(this)

    //note, return type Fq was changed into Any
    def modsqrt: Fq = 
        if value.toInt == 0 then
            Fq(Q, 0)
        else if (value.pow((Q - 1) / 2) % Q != 1) then
            throw new Exception("No sqrt exists")
        else if (Q % 4 == 3) then
            Fq(Q, math.pow(value.toInt, (Q + 1) / 4).toInt % Q)
        else if (Q % 8 == 5) then
            Fq(Q, math.pow(value.toInt, (Q + 3) / 8).toInt % Q)
            // p % 8 == 1. Tonelli Shanks algorithm for finding square root
        else {
        var S = 0
        var q = Q - 1
        while (q % 2 == 0)
        do
            q = q / 2
            S += 1

        var i = 0
        var z = 0
        var found = false
        while (i < Q && !found) {
            val euler = Math.pow(i, (Q - 1) / 2).toInt % Q
            if (euler == (-1 % Q)) {
                z = i
                found = true
            }
            i += 1
        }

        var M = S
        var c = math.pow(z, q).toInt % Q
        var t = math.pow(value.toInt, q).toInt % Q
        var R = math.pow(value.toInt, (q + 1) / 2).toInt % Q

        def loop(t: Int): Fq = t match
          case 0 => Fq(Q, 0)
          case 1 => Fq(Q, R)
          case _ =>
            def subloop(f: BigInt, i: Int): Int =
              if f == 1 then i else
                val f1 = f.pow(2) % Q
                subloop(f1, i + 1)
            val i = subloop(t, 0)

            val b = math.pow(c, math.pow(2, M - i - 1).toInt % Q).toInt % Q
            M = i
            c = math.pow(b, 2).toInt % Q
            val t1 = (t * c) % Q
            R = (R * b) % Q
            loop(t1)
        loop(t)
    }


    def copy: Fq = new Fq(Q, value)
    

object Fq {
    def zero(Q: Int): Fq = new Fq(Q, 0)

    def one(Q: Int): Fq = new Fq(Q, 1)

    def fromFq(Q: Int, fq: Fq): Fq = fq
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

