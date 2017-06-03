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

  test("times(List (a, b, a)) == (a,2), (b,1)") {
    new TestTrees {
      val result = times(List('a', 'b', 'a'))
      assert( result.head === ('a', 2))
      assert( result.tail.head === ('b', 1))
      assert( result.size === 2)
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("createCodeTree") {
    val leaflist:CodeTree = createCodeTree('k'::'o'::'k'::'o'::'s'::Nil)
    println(leaflist)
    assert(chars(leaflist) === 's'::'k'::'o'::Nil)
    assert(weight(leaflist) === 5)
  }


  test("test decode french code") {
    new TestTrees {
      println(decodedSecret)
    }
  }

  test("test encode kokos") {
    new TestTrees {
      val ct = Fork(Fork(Leaf('s',1),Leaf('k',2),List('s', 'k'),3),Leaf('o',2),List('s', 'k', 'o'),5)
      val bits = encode(ct)("kokos".toList)
      assert(bits === List(0,1,1,0,1,1,0,0))
    }
  }

  test("test encode on secret") {
    new TestTrees {
      assert( secret === encode(frenchCode)(decodedSecret) )
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    new TestTrees {
      val table = ('a', List(1,1,1,1))::('b', List(0,0,0))::('k', List(1,1,1))::Nil
      assert( codeBits(table) ('k') !== Nil)
      assert( codeBits(table) ('k') === List(1,1,1))
    }
  }

  test("convert") {
    new TestTrees {
      val ct = Fork(Fork(Leaf('s',1),Leaf('k',2),List('s', 'k'),3),Leaf('o',2),List('s', 'k', 'o'),5)
      assert(List(('s',List(0, 0)), ('k',List(0, 1)), ('o',List(1)) ) === convert(ct))
      println (convert(ct))
    }
  }
}
