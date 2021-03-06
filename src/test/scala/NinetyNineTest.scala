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

  "P17 split" should {
    val orig = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val split = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    val split2 = (Nil,List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

    "create a new tuple " + split.toString + " from " + orig.toString + "for 3" in{
      NN.split(3, orig) must_== split
    }
    "create a new tuple " + split.toString + " from " + orig.toString + " for 0" in{
      NN.split(0, orig) must_== split2
    }
  }

  "P18 slice" should {
    val orig = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val slice = List('d, 'e, 'f, 'g)

    "create a new list " + slice.toString + " from " + orig.toString in{
      NN.slice(3, 7, orig) must_== slice
    }
  }

  "P19 rotate" should {
    val orig = List('a, 'b, 'c,   'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)     //len:11
    val rot1 = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k,   'a, 'b, 'c)
    val rot2 = List('j, 'k,   'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

    "rotate " + orig.toString + " 3 places to create " + rot1.toString in{
      NN.rotate(3, orig) must_== rot1
    }
    "rotate " + orig.toString + " -2 places to create " + rot2.toString in{
      NN.rotate(-2, orig) must_== rot2
    }
  }

  "P20 remove" should {
    val orig = List('a, 'b, 'c, 'd)
    val end = (List('a, 'c, 'd),'b)

    "remove element b from  " + orig.toString + " and return " + end.toString in{
      NN.removeAt(1, orig) must_== end
    }
  }

  "P21 insert" should {
    val orig = List('a, 'b, 'c, 'd)
    val end = List('a, 'new, 'b, 'c, 'd)

    "insert element 'new into  " + orig.toString + " and return " + end.toString in{
      NN.insertAt('new, 1, orig) must_== end
    }
  }

  "P22 range" should {
    val end = List(4, 5, 6, 7, 8, 9)

    "given 4 and 9 create a list " + end.toString in{
      NN.range(4, 9) must_== end
    }
  }

  "P23 random" should {
    val start = List('a, 'b, 'c, 'd, 'f, 'g, 'h)

    "given 3 create a list of 3 random elements from" + start.toString in{
      NN.randomSelect(3, start).length must_== 3

    }
  }

  "P24 lotto" should {
    val nums = 3

    val upperBound = 50

    val end = NN.lotto(nums,upperBound)
    "given 3 create a list of "+nums+" random elements " in{
      NN.length(end) must_== nums
    }
    "and send unique results" in {
      end.toSet.size must_== end.length
    }

    "and all elements are bounded by the range" in {
      end.foldLeft(0)( (x, y) => if (y >= 1 && y <= upperBound) x + 1 else x) must_== end.length
    }
  }

  //test random?
  "P25 random permutation" should {
    val start = List(1, 2, 3, 4, 5, 6)

    val end = NN.randomPermute(start)

    "given a list of elements the permutation length must contain the same elements" in{
      end.sortWith(_ > _) must_== start.sortWith(_ > _)
    }
  }

  "P26 Combinations" should {
    val start = List('a, 'b, 'c)
    val end = List(List('b,'c), List('a,'c), List('c,'b), List('a, 'b), List('c, 'a), List('b, 'a))
    "given " + start.toString + " should generate " + end.toString in {
      NN.combinations(2, start) must_== end
    }

    val start2 = List(1,2,3,4,5,6,7,8,9,10,11,12)

    val endVal = 220
    "given " + start2 + " the length of the combinations list will be " + 220 in {
      NN.combinations(3, start2).length must_== endVal
    }
  }

}