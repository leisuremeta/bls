// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
package bls

class MySuite extends munit.FunSuite {
  test("fields.scala") {
    assertEquals(10,10)
    val a = Fq(17, 30)
    val b = Fq(17, -18)
    val c = Fq2(17, a, b)
    val d = Fq2(17, a + a, Fq(17, -5))
    val e = c * d
    val f = e * d
    assertEquals( f != e, true)
    val eSq = e * e
    val eSqrt = eSq.modsqrt
    assertEquals( eSqrt.pow(2), eSq)

  }
}


