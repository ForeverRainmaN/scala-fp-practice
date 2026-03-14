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
}
