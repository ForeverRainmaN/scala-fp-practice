package com.rockthejvm.numbers

import scala.annotation.tailrec

object ReverseInteger extends App {
  // return a number with the digits reversed
  // if the result overflows Int, return 0
  def reverseInteger(number: Int): Int = {
    @tailrec
    def go(remaining: Int, acc: String): Int = {
      if (remaining <= 0) acc.toIntOption match
        case Some(value) => value
        case None        => 0
      else {
        val last = remaining % 10
        val newAcc = acc + last
        go(remaining / 10, newAcc)
      }

    }
    go(number, "")
  }

  def reverseIntegerV2(number: Int): Int = {
    @tailrec
    def go(remaining: Int, acc: Int): Int = {
      if (remaining == 0) acc
      else {
        val digit = remaining % 10
        val tentativeResult = acc * 10 + digit

        if (acc >= 0 != (tentativeResult >= 0)) 0
        else go(remaining / 10, tentativeResult)
      }
    }
    // -2^31...2^31-1
    if (number == Int.MinValue) 0
    else if (number >= 0) go(number, 0)
    else -go(-number, 0)
  }

  println(reverseIntegerV2(-157))
}
