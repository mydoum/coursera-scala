package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val ct = List(('a', List(0, 1, 1)), ('b', List(1, 0, 0)))
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times of a char occurence in a list") {
    assert(times(List('a', 'b', 'a')) == List(('a', 2), ('b', 1)) || times(List('a', 'b', 'a')) == List(('b', 1), ('a', 2)), "Times 1")
  }

  test("times of chars occurence in a more complex list") {
    val l = times(string2Chars("hello, world"))
    assert(l.contains(('l', 3)))
    assert(l.contains(('o', 2)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(until(singleton, combine)(leaflist) == Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3))
  }

  test("create a code tree") {
    new TestTrees {
      val chars = string2Chars("aabbbdddd")
      assert(createCodeTree(chars) == t2)
    }
  }

  test("decode secret") {
    println(decodedSecret)
    assert(true)

  }

  test("decode simple") {
    new TestTrees {
      assert(decode(t1, List(1, 0, 1)) == List('b', 'a', 'b'))
    }
  }

  test("encode simple") {
    new TestTrees {
      assert(encode(t1)(List('b', 'a', 'b')) == List(1, 0, 1), "test with t1")
      assert(encode(t2)(List('a', 'd')) == List(0, 0, 1), "test with t2")
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abb".toList)) === "abb".toList)
    }
  }

  test("codeTable test") {
    new TestTrees {
      assert(codeBits(ct)('a') == List(0, 1, 1))
    }
  }

  test("convert a codeTree into an operational codeTable") {
   new TestTrees {
     assert(convert(t2) == List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
   }
  }

  test("mergeCodeTables") {
    assert(mergeCodeTables(List(('a', List(0, 1, 1))), List(('b', List(1, 0, 0)))) == List(('a', List(0, 1, 1)), ('b', List(1, 0, 0))))
  }

  test("quickEncode") {
    new TestTrees {
      assert(quickEncode(t1)(List('b', 'a', 'b')) == List(1, 0, 1), "test with t1")
      assert(quickEncode(t2)(List('a', 'd')) == List(0, 0, 1), "test with t2")
    }
  }

}
