package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (8, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("badLink") = forAll { (a: A, b: A) =>
    (a < b) ==> {
      val heap = empty
      val fullHeap = insert(a, insert(b, heap))
      findMin(deleteMin(fullHeap)) == b
    }
  }

  property("passAll") = forAll { (a: A, b: A, c: A) =>
    def bigger(x: A, y: A): A = {
      if (ord.gt(x, y)) x
      else y
    }

    def biggest(x: A, y: A, z: A): A = bigger(x, bigger(y, z))

    val max = biggest(a, b, c)
    val mid = {
      if (a == max) bigger(b, c)
      else if (b == max) bigger(a, c)
      else bigger(a, b)
    }
    val oneHeap = insert(c, empty)
    val twoHeap = insert(a, oneHeap)
    val threeHeap = insert(b, twoHeap)
    val minFound = findMin(deleteMin(threeHeap))
    minFound == mid
  }

}
