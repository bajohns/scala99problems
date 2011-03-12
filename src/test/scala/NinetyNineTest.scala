/**
 * Created by IntelliJ IDEA.
 * User: bajohns
 * Date: 3/11/11
 * Time: 3:20 PM
 * To change this template use File | Settings | File Templates.
 */


import com.scalaproblems.NinetyNine
import org.specs._

import com.scalaproblems._

class NinetyNineTest extends Specification {
  val xs = List(1,2,3,4,5)

  "problem one last element" should {
        " be 5" in {
          NinetyNine.last(xs) must_== Some(5)
        }
  }

  "problem two penultimate element" should {
      "be 4" in {
        NinetyNine.penultimate(xs) must_== Some(4)
      }
  }

  "problem three kth element" should {
        "be 2 for k=2" in {
            NinetyNine.elementAt(2, xs) must_== Some(2)
        }
        "be None for out of bounds request" in {
          NinetyNine.elementAt(xs.length + 1, xs) must_== None
        }
  }

  "problem four length" should {
      "be 5 for list xs" in {
        NinetyNine.length(xs) must_== 5
      }
  }

   "problem five reverse" should {
      "be (5,4,3,2,1) for list (1,2,3,4,5)" in {
        NinetyNine.reverse(xs) must_== List(5,4,3,2,1)
      }
    }

   "problem six palindrome" should {
      "be true for List (1,2,3,3,2,1)" in {
        NinetyNine.isPalindrome(List(1,2,3,3,2,1)) must_== true
      }

     "be false for List (1,2,3)" in {
        NinetyNine.isPalindrome(List(1,2,3)) must_== false
      }
    }

    "problem seven flatten list" should {
      val xFlatten = List(List(1,2), List(3,4,5))

      "return List(1,2,3,4,5) for " + xFlatten.toString()  in {
        NinetyNine.flatten(xFlatten) must_== List(1,2,3,4,5)
      }
    }

    "problem eight compress" should {
      val xCompress = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val xCompressed = List('a, 'b, 'c, 'a, 'd, 'e)
      "remove duplicates from " + xCompress.toString + " to " + xCompressed.toString in {
        NinetyNine.compress(xCompress) must_== xCompressed
      }


    }
}