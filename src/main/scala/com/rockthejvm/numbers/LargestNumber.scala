package com.rockthejvm.numbers

import scala.annotation.tailrec

object LargestNumber {
  /*
    given a list of non-negative integers, arrange them such that they form the largest number
    the result might be huge so return a string.

    list(10, 2) => "210"
    list(3, 30, 5, 9, 34) => "9534330"
    list(3, 5, 9, 30, 34)
   */

  def largestNumber(numbers: List[Int]): String = numbers.sortWith { (a, b) =>
    s"$a$b" > s"$b$a"
  }.mkString

  def largestNumberV2(numbers: List[Int]): String = {
    given newOrdering: Ordering[Int] = {
      Ordering.fromLessThan { (a, b) =>
        val aStr = a.toString
        val bStr = b.toString
        (aStr + bStr).compareTo(bStr + aStr) >= 0
      }
    }

    val largest = numbers.sorted.mkString("")

    if (largest.charAt(0) == '0') "0" else largest
  }
}
