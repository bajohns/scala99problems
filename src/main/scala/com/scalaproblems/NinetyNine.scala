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
      case x if count == 0 => x.headOption
      case x :: Nil if count > 0 => None
      case x :: tail if count > 0 => findKthElem(count -1, tail)
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






  def group[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => Nil
    case x :: tail => {
      val (a, b) = tail.span(_ == x)
      (x :: tail.takeWhile(_ == x)) :: pack(tail)
      (x :: a) :: group(b)
    }
  }
  
  def pack[A](xs: List[A]): List[A] = group(xs) map ( _.head )
  
  def encode[A](xs: List[A]): List[(Int, A)] = group(xs) map ( ys => (length(ys), ys.head) )
  
  def encodeModified[A](xs: List[A]): List[Any] = group(xs) map ( _ match {
    case List(x) => x
    case ys => (length(ys), ys.head)
  })
  
  def decode[A](xs: List[(Int, A)]): List[A] = xs flatMap { 
    case (n, x) => Stream.fill(n)(x)
  }
  
  def encodeDirect[A](xs: List[A]): List[(Int, A)] = undefined
  
  def duplicate[A](xs: List[A]): List[A] = undefined
  
  def duplicateN[A](n: Int, xs: List[A]): List[A] = undefined
  
  def dropN[A](n: Int, xs: List[A]): List[A] = undefined
  
  def split[A](n: Int, xs: List[A]): (List[A], List[A]) = undefined
  
  // scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g)
  def slice[A](start: Int, end: Int, xs: List[A]): List[A] = undefined
  
  // rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  def rotate[A](n: Int, xs: List[A]): List[A] = undefined
  
  // P20
  def removeAt[A](n: Int, xs: List[A]): List[A] = undefined
  
  // P21
  def insertAt[A](x: A, n: Int, xs: List[A]): List[A] = undefined
  
  // P22
  def range(start: Int, end: Int): List[Int] = undefined
  
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
  def isPrime(n: Int): Boolean = {
    def sieve(s: Stream[Int]): Stream[Int] = Stream.cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))
    def primes: Stream[Int] = Stream.cons(2, Stream.from(3, 2).filter(x => isPrime(x)))
    
    primes.takeWhile(_ <= Math.sqrt(n)).forall(n % _ != 0)
  }
  
  // p32: Determine the greatest common divisor of two positive integer numbers.
  // Hint: Euclidean or Binary (Stein's)
  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)  
  
  // p33: Determine whether two positive integer numbers are coprime.
  def coprime(m: Int, n: Int): Boolean = gcd(m, n) == 1
  
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
  def primesInRange(r: Range): List[Int] = r filter (isPrime(_)) toList
  
  
}
