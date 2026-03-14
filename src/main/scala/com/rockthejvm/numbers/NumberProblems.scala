package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumberProblems extends App {
  def isPrime(n: Int): Boolean = {
    @tailrec
    def go(possibleDivisor: Int): Boolean = {
      if (possibleDivisor <= 1) true
      else if (n % possibleDivisor == 0) false
      else go(possibleDivisor - 1)
    }

    if (n < 2) false else go(n / 2)
  }
  // Complexity: O(sqrt(n))
  def isPrimeV2(n: Int): Boolean = {
    @tailrec
    def go(currentDivisor: Int): Boolean = {
      if (currentDivisor > Math.sqrt(Math.abs(n))) true
      else n % currentDivisor != 0 && go(currentDivisor + 1)
    }

    go(2)
  }
  // the constituent prime divisors
  def decompose(n: Int): List[Int] = {
    @tailrec
    def go(curr: Int, currDiv: Int, acc: List[Int]): List[Int] = {
      if (curr <= 1) acc.reverse
      else if (curr % currDiv != 0) go(curr / currDiv, currDiv, currDiv :: acc)
      else go(curr, currDiv + 1, acc)
    }

    if (isPrime(n)) List(n) else go(n, 2, List())
  }

  // Complexity: O(sqrt(n))
  def decomposeV2(n: Int): List[Int] = {
    assert(n > 0)

    @tailrec
    def go(remaining: Int, currentDivisor: Int, acc: List[Int]): List[Int] = {
      if (currentDivisor > Math.sqrt(remaining)) remaining :: acc
      else if (remaining % currentDivisor == 0)
        go(remaining / currentDivisor, currentDivisor, currentDivisor :: acc)
      else go(remaining, currentDivisor + 1, acc)
    }

    go(n, 2, List())
  }
}
