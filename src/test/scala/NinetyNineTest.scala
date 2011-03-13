/**
 * Created by IntelliJ IDEA.
 * User: bajohns
 * Date: 3/11/11
 * Time: 3:20 PM
 * To change this template use File | Settings | File Templates.
 */


import com.scalaproblems.{NinetyNine => NN}
import org.specs._

import com.scalaproblems._

class NinetyNineTest extends Specification {
  val xs = List(1,2,3,4,5)

  "P01 last element" should {
        " be 5" in {
          NN.last(xs) must_== Some(5)
        }
  }

  "P02 penultimate element" should {
      "be 4" in {
        NN.penultimate(xs) must_== Some(4)
      }
  }

  "P03 kth element" should {
        "be 2 for k=2" in {
            NN.elementAt(2, xs) must_== Some(2)
        }
        "be None for out of bounds request" in {
          NN.elementAt(xs.length + 1, xs) must_== None
        }
  }

  "P04 length" should {
      "be 5 for list xs" in {
        NN.length(xs) must_== 5
      }
  }

   "P05 reverse" should {
      "be (5,4,3,2,1) for list (1,2,3,4,5)" in {
        NN.reverse(xs) must_== List(5,4,3,2,1)
      }
    }

   "P06 palindrome" should {
      "be true for List (1,2,3,3,2,1)" in {
        NN.isPalindrome(List(1,2,3,3,2,1)) must_== true
      }

     "be false for List (1,2,3)" in {
        NN.isPalindrome(List(1,2,3)) must_== false
      }
    }

    "P07 flatten list" should {
      val xFlatten = List(List(1,2), List(3,4,5))

      "return List(1,2,3,4,5) for " + xFlatten.toString()  in {
        NN.flatten(xFlatten) must_== List(1,2,3,4,5)
      }
    }

    "P08 compress" should {
      val xCompress = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val xCompressed = List('a, 'b, 'c, 'a, 'd, 'e)
      "remove duplicates from " + xCompress.toString + " to " + xCompressed.toString in {
        NN.compress(xCompress) must_== xCompressed
      }
    }

    val xPacked = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    val xPack = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    "P09 pack" should {
      "create a new list " +  xPacked.toString + " from " + xPack.toString in {
        NN.pack(xPack) must_== xPacked
      }

    }

    val xEncoded = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    "P10 encode" should {
      "create a new list " +  xEncoded.toString + " from " + xPack.toString in {
        NN.encode(xPack) must_== xEncoded
      }
    }

    val xEncodedModified = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    "P11 encode modified" should {
      "create a new list " +  xEncodedModified.toString + " from " + xPack.toString in {
        NN.encodeModified(xPack) must_== xEncodedModified
      }
    }

    "P12 decode" should {
      "create a new list " +  xPack.toString + " from " + xEncoded.toString in {
        NN.decode(xEncoded) must_== xPack
      }
    }

    "P13 decode" should {
      "create a new list " +  xEncoded.toString + " from " + xPack.toString in {
        NN.encodeDirect(xPack) must_== xEncoded
      }
    }


    val toTrans = List('a, 'b, 'c, 'c, 'd)
    "P14 duplicate" should{
      val dup = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
      "create a new list " + dup.toString + " from " + toTrans.toString in {
        NN.duplicate(toTrans) must_== dup
      }
    }

    "P15 duplicateN" should{
      val trip = List('a, 'a, 'a,
                      'b, 'b, 'b,
                      'c, 'c, 'c,
                      'c, 'c, 'c,
                      'd, 'd, 'd)
      val quad = List('a, 'a, 'a, 'a,
                      'b, 'b, 'b, 'b,
                      'c, 'c, 'c, 'c,
                      'c, 'c, 'c, 'c,
                      'd, 'd, 'd, 'd)
      "create a new list " + trip.toString + " from " + toTrans.toString in {
        NN.duplicateN(3,toTrans) must_== trip
      }
      "create a new list " + quad.toString + " from " + toTrans.toString in {
        NN.duplicateN(4,toTrans) must_== quad
      }
    }

    "P16 dropN" should {
      val orig = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val reduced = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

      "create a new list " + reduced.toString + " from " + orig.toString in{
        NN.dropN(3, orig) must_== reduced
      }
    }
}