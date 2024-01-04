package bls

import io.github.iltotore.iron.autoRefine

class MySuite extends munit.FunSuite:
  test("fields.scala"):
    val a = Fq(17, 30)
    val b = Fq(17, -18)
    val c = Fq2(17, a, b)
    val d = Fq2(17, a + a, Fq(17, -5))
    val e = c * d
    val f = e * d
    assertEquals(f != e, true)

    val eSq = e * e
    val eSqrt = eSq.modsqrt
    val eSqrtPow2 = eSqrt.pow(2)
    assertEquals(eSqrtPow2, eSq)
