package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(1)
    val s6 = singletonSet(-1)
    val multSet1 = union(s1, s2)
    val multSet2 = union(s1, s3)
    val multSet3 = union(s3, s4)


    var megaSet = singletonSet(0)
    var negSet = singletonSet(-1)
    var posSet = singletonSet(1)
    for (i <- -100 to 100) {
      megaSet = union(megaSet, singletonSet(i))
      if (i < 0) negSet = union(negSet, singletonSet(i))
      if (i > 0) posSet = union(posSet, singletonSet(i))
    }
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      println(printSet(megaSet))
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s4, 1), "Singleton false")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains elements") {
    new TestSets {
      val sInt1 = intersect(s1, s2)
      val sInt2 = intersect(s1, s5)
      assert(!contains(sInt1, 1), "Intersect 1")
      assert(contains(sInt2, 1), "Intersect 2")
    }
  }

  test("diff contains all elements in s that are not in t") {
    new TestSets {
      val s = diff(multSet1, multSet2)
      assert(contains(s, 2), "Diff 1")
      assert(!contains(s, 1), "Diff 2")
    }
  }

  test("Returns whether all bounded integers within `s` satisfy `p`.") {
    new TestSets {
      val filter = (x: Int) => x < 0
      assert(forall(negSet, filter), "forall 1")
      assert(!forall(s5, filter), "forall 2")
      assert(forall(s6, filter), "forall 3")
    }
  }

  test("Returns whether a bounded integers within `s` satisfy `p`.") {
    new TestSets {
      val filter = (x: Int) => x < 0
      assert(exists(megaSet, filter), "Exists 1")
      assert(!exists(s5, filter), "Exists 2")
      assert(exists(s6, filter), "Exists 3")
    }
  }

  test("Returns a set transformed by applying `f` to each element of `s`.") {
    new TestSets {
    }
  }
}
