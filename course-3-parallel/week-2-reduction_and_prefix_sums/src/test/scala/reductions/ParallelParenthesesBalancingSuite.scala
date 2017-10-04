package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {
  val threshold = 10

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    def checkPar(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected,
        s"balancePar($input) should be $expected")

    check("", true)
    checkPar("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance: $input should be $expected")

    def checkPar(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected,
        s"balancePar: $input should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)

    checkPar("(", false)
    checkPar(")", false)
    checkPar(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance: $input should be $expected")

    def checkPar(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected,
        s"balancePar: $input should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)

    checkPar("()", true)
    checkPar(")(", false)
    checkPar("((", false)
    checkPar("))", false)
    checkPar(".)", false)
    checkPar(".(", false)
    checkPar("(.", false)
    checkPar(").", false)
  }

  test("check only ParBalance") {
    def checkPar(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected,
        s"balance($input) should be $expected")

    checkPar("()()()()()()()()()()", true)
  }


}