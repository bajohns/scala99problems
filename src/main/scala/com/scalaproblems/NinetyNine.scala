package com.scalaproblems

// http://aperiodic.net/phil/scala/s-99/
object NinetyNine {
  val xs = List(1,2,3,4,5)

  def undefined = error("undefined")


  def last[A](xs: List[A]): Option[A] = xs match {
    case x :: Nil => Some(x)
    case _ :: tail => last(tail)
    case _ => None
  }

  def penultimate[A](xs: List[A]): Option[A] = xs match {
      case x :: _ :: Nil => Some(x)
      case x :: tail => penultimate(tail)
      case _ => None
  }
  
  def elementAt[A](n:Int, xs: List[A]): Option[A] =    {
    def findKthElem(count:Int, list: List[A]): Option[A] =   list match {
      case Nil => None
      case x :: Nil  => if (count == 0) Some(x) else None
      case x :: tail => if (count == 0) Some(x) else findKthElem(count -1, tail)
    }
    if(n > xs.length) None
    else findKthElem(n -1, xs)
  }
  
  def length[A](xs: List[A]): Int = {
    def lengthAcc(acc:Int, xs:List[A]): Int = xs match {
      case Nil => acc
      case _ :: Nil => acc +1
      case _ :: tail => lengthAcc(acc + 1, tail)
    }
    lengthAcc(0, xs)
  }
  
  def reverse[A](xs: List[A]): List[A] = {
    def reverseHelper (revList: List[A], origList:List[A]):List[A] = origList match {
      case Nil => revList
      case x :: tail => reverseHelper(x :: revList, tail)
    }
    reverseHelper(Nil, xs)
  }

  def isPalindrome[A](xs: List[A]) = xs == reverse (xs)
  
  def flatten[A](xs: List[Any]): List[Any] = xs flatMap {
    case x: List[_] => flatten(x)
    case x => List(x)
  }
  
  def compress[A](xs: List[A]): List[A] = {
    def compressHelper(acc: List[A], orig: List[A]):List[A] = orig match {
      case Nil => acc
      case x :: Nil => reverse(x :: acc)
      case x :: y :: tail  => if (x == y) compressHelper(acc, x :: tail)
                            else compressHelper(x :: acc,  y :: tail)
    }
    compressHelper(Nil, xs)
  }



  def pack[A](xs: List[A]): List[List[A]] = {
    def packHelper(rss:List[List[A]], acc:List[A], orig:List[A], cur: A):List[List[A]] = {
        orig match{
          case Nil => reverse(acc :: rss)
          case x :: Nil => if (x == cur) packHelper(rss, x :: acc, Nil, x)
                        else  packHelper(acc :: rss , List(x), Nil, x)
          case x :: tail => if (x == cur) packHelper(rss, x :: acc, tail, x)
                        else  packHelper(acc :: rss , List(x), tail, x)
        }
    }
    if (!xs.isEmpty) packHelper(Nil, Nil, xs, xs.head) else throw new NoSuchElementException
  }
  
  def encode[A](xs: List[A]): List[(Int, A)] = {
    pack(xs) map { x => (length(x), x.head)}
  }
  
  def encodeModified[A](xs: List[A]): List[Any] = {
    pack(xs) map { x =>
        if(length(x) == 1) x.head
        else (length(x), x.head)
    }
  }

  //special
  def accumulate[A] (acc: List[A], count:Int, symbol:A):List[A] = {
    if (count == 0) acc else accumulate(symbol :: acc, count - 1, symbol)
  }

  def decode[A](xs: List[(Int, A)]): List[A] = {

    xs flatMap {
        case (count, symbol) => accumulate(Nil, count, symbol)
    }
  }
  
  def encodeDirect[A](xs: List[A]): List[(Int, A)] = {
    def packHelper(ret:List[(Int, A)], acc:Int, orig:List[A], cur: A): List[(Int, A)] = {
        orig match{
          case Nil => ret
          case x :: Nil => if (x == cur) reverse((acc +1, x) :: ret)
                        else  reverse((1, x) :: (acc, cur) :: ret)
          case x :: tail => if (x == cur) packHelper(ret, acc +1, tail, x)
                        else  packHelper((acc, cur) :: ret , 1, tail, x)
        }
    }
    if (!xs.isEmpty) packHelper(Nil, 0, xs, xs.head) else throw new NoSuchElementException
  }
  
  def duplicate[A](xs: List[A]): List[A] = {
    duplicateN(2,xs)
  }
  
  def duplicateN[A](n: Int, xs: List[A]): List[A] = {
    val rev = reverse(xs)
    rev.tail.foldLeft(accumulate(Nil, n, rev.head)) { (acc, x) => accumulate (acc, n, x)}
  }
  def dropN[A](n: Int, xs: List[A]): List[A] = {
     def filterHelper (pos:Int, acc: List[A], orig:List[A]):List[A] = (pos, orig) match {
       case (_, Nil) => reverse(acc)
       case (1, h :: tail ) => filterHelper(n,acc, tail)
       case (_, h :: tail ) => filterHelper(pos -1, h :: acc, tail)
     }
     filterHelper(n, Nil, xs)
  }
  
  def split[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    def splitHelper(pos:Int, hs:List[A], ts:List[A]):(List[A], List[A]) = (pos, ts) match{
      case (_ , Nil) => (reverse(hs), Nil)
      case (0, x :: tail) => (reverse(x :: hs), tail)
      case (_, x :: tail) => splitHelper(pos -1 , x :: hs, tail)
    }
    splitHelper(n-1, Nil, xs)
  }

  def slice[A](start: Int, end: Int, xs: List[A]): List[A] = {

    def sliceHelper(s:Int, e:Int, ss:List[A], ts:List[A]):List[A] = (s, e, ts) match{
      case (_, _ , Nil) => reverse(ss)
      case (0, 0, x :: tail) => reverse(ss)
      case (0, ne, x :: tail) => sliceHelper(0, ne -1 , x :: ss, tail)
      case (ns, ne, x :: tail) => sliceHelper(ns -1 , ne -1 , ss, tail)
    }
    sliceHelper(start, end, Nil, xs)
  }
  def rotate[A](n: Int, xs: List[A]): List[A] = {
    val len = length(xs)
    val cut = if (n >= 0)  n else len + n

    slice(cut, len, xs) ::: slice(0,cut,xs)
  }
  
  // P20
  def removeAt[A](n: Int, xs: List[A]): (List[A], A) = split(n,xs) match {
    case (_, Nil) => throw new NoSuchElementException
    case (hs, x :: ts) => (hs ::: ts, x)
  }
  // P21
  //Inefficient - revise
  def insertAt[A](x: A, n: Int, xs: List[A]): List[A] = split(n, xs) match{
    case (hs, ts) => hs ::: x :: ts
  }
  // P22
  def range(start: Int, end: Int): List[Int] = {

    def rangeHelper(num:Int, xs:List[Int]):List[Int] = {
      if( num < start) xs
      else rangeHelper(num -1, num :: xs)
    }
    rangeHelper(end, Nil)
  }
  // p23
  def randomSelect[A](n: Int, xs: List[A]): List[A] = undefined
  
  // p24
  def lotto(n: Int, m: Int): List[Int] = undefined
  
  // p25
  def randomPermute[A](xs: List[A]): List[A] = undefined
  
  // p26
  def combinations[A](k: Int, xs: List[A]): List[List[A]] = undefined
  
  // p27
  def group[T](ns: List[Int], l: List[T]): List[List[List[T]]] = undefined
  
  // p28
  def lsort[A](xs: List[List[A]]): List[List[A]] = undefined
  
  // p31: Determine whether a given integer number is prime.
  def isPrime(n: Int): Boolean = undefined
  
  // p32: Determine the greatest common divisor of two positive integer numbers.
  // Hint: Euclidean or Binary (Stein's)
  def gcd(m: Int, n: Int): Int = undefined
  
  // p33: Determine whether two positive integer numbers are coprime.
  def coprime(m: Int, n: Int): Boolean = undefined
  
  // p34: Calculate Euler's totient function phi(m)
  def totient(n: Int): Int = undefined
  
  // p35: Determine the prime factors of a given positive integer
  def primeFactors(n: Int): List[Int] = undefined
  
  // p36: Determine the prime factors of a given positive integer (2).
  def primeFactorMultiplicity(n: Int): List[(Int, Int)] = undefined
  
  // p37: Calculate Euler's totient function phi(m) (improved).
  def totient2(n: Int): Int = undefined
  
  // p38: Compare the two methods of calculating Euler's totient function
  
  // p39: 
  def primesInRange(r: Range): List[Int] = undefined
  
  
}
