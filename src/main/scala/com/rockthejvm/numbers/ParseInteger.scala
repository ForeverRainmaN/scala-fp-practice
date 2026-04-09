package com.rockthejvm.numbers

import scala.annotation.tailrec

object ParseInteger {
  /*
    Return a number from the string argument:
        - there may be leading spaces, ignore those
        - read the sign charecter if present
        - read all the digits until the end of the string or until a non-digit charecter
        - return the number formed from those digits
        - if the number exceeds the int range, return either Int.MinValue(underflow) or Int.MaxValue(overflow)
 
   */

  /*
    "+1234" => 1234
    "-1234" => -1234
    "   +1234 is the number I want" => 1234

   */

  def isMinusSign(c: Char): Boolean = c == '-'
  def isPlusSign(c: Char): Boolean = c == '+'

  def parseInteger(str: String): Int = {
    val parsed = str.trim
    val first = parsed.head
    @tailrec
    def go(acc: Int, remaining: String, isNegative: Boolean): Int = {
      if (remaining.isEmpty) acc
      else if (!remaining.head.isDigit) acc
      else {
        val headToInt = remaining.head.asDigit
        if (acc > (Int.MaxValue - headToInt) / 10) {
          if (isNegative) Int.MinValue else Int.MaxValue
        } else {
          val newAcc = acc * 10 + headToInt
          go(newAcc, remaining.tail, isNegative)
        }
      }
    }

    if (isMinusSign(first)) go(0, parsed.tail, true)
    else if (isPlusSign(first)) go(0, parsed.tail, false)
    else go(0, parsed, false)
  }

  def parseIntegerV2(string: String): Int = {
    val WHITESPACE = ' '
    val PLUS = '+'
    val MINUS = '-'
    val DIGITS = "0123456789".toSet

    def integerRangeEnd(sign: Int): Int =
      if (sign >= 0) Int.MaxValue else Int.MinValue

    @tailrec
    def parseTailRec(remainder: String, sign: Int, acc: Int = 0): Int = {
      if (remainder.isEmpty || !DIGITS.contains(remainder.charAt(0))) acc
      else {
        val newDigit = remainder.charAt(0) - '0'
        val tentativeResult = acc * 10 + newDigit * sign

        if ((sign >= 0) != (tentativeResult >= 0)) integerRangeEnd(sign)
        else parseTailRec(remainder.substring(1), sign, tentativeResult)
      }
    }

    if (string.isEmpty) 0
    else if (string.charAt(0) == WHITESPACE) parseIntegerV2(string.substring(1))
    else if (string.charAt(0) == PLUS)
      parseTailRec(string.substring(1), sign = 1)
    else if (string.charAt(0) == MINUS)
      parseTailRec(string.substring(1), sign = -1)
    else parseTailRec(string, sign = 1)
  }
}
