package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PolynomialSuite extends FunSuite {
  test("computeDelta with positive polynomial root") {
    val result = Polynomial.computeDelta(Var(2.0), Var(5.0), Var(3.0))
    assert(result() == 1.0)
  }

  test("computeSolutions") {
    val a = Var(3.0)
    val b = Var(-6.0)
    val c = Var(3.0)
    val result = Polynomial.computeSolutions(a, b, c, Polynomial.computeDelta(a, b, c))

    assert(result().forall(res => res == 1.0), "test with null discriminant")

    val d = Var(1.0)
    val e = Var(2.0)
    val f = Var(5.0)

    val result2 = Polynomial.computeSolutions(d, e, f, Polynomial.computeDelta(d, e, f))

    assert(result2().isEmpty, "test with negative discriminant")

    val g = Var(3.0)
    var h = Var(5.0)
    var i = Var(2.0)

    val result3 = Polynomial.computeSolutions(g, h, i, Polynomial.computeDelta(g, h, i))

    assert(result3().contains(-1.0) && result3().contains(-2.0/3.0))
  }
}
