package com.rockthejvm.strings

import scala.annotation.tailrec
import scala.collection.mutable.Stack

object ParenthesisProblems extends App {
  /*
        "()" => true
        "()()" => true
        "(())" => true
        ")(" => false
   */
  def hasValidParentheses(string: String): Boolean = {
    val pairs = Map(')' -> '(', '}' -> '{', ']' -> '[')

    @tailrec
    def go(remaining: String, stack: Stack[Char]): Boolean = {
      if (remaining.isEmpty) stack.isEmpty
      else {
        val char = remaining.head
        if (pairs.values.exists(_ == char)) {
          stack.push(char)
          go(remaining.tail, stack)
        } else if (pairs.contains(char)) {
          if (stack.isEmpty || stack.pop() != pairs(char)) false
          else go(remaining.tail, stack)
        } else {
          go(remaining.tail, stack)
        }
      }
    }

    go(string, Stack.empty)
  }

  def hasValidParenthesesV2(string: String): Boolean = {
    val pairs = Map(')' -> '(', '}' -> '{', ']' -> '[')

    string
      .foldLeft(Option(List.empty[Char])) { (accOpt, char) =>
        accOpt.flatMap { stack =>
          char match {
            case '(' | '{' | '[' => Some(char :: stack)
            case ')' | '}' | ']' =>
              stack.headOption.flatMap { top =>
                if (top == pairs(char)) Some(stack.tail)
                else None
              }
            case _ => Some(stack)
          }
        }
      }
      .exists(_.isEmpty)
  }
  // Complexity: O(N)
  def hasValidParenthesesV3(string: String): Boolean = {
    @tailrec
    def go(remaining: String, openParens: Int): Boolean = {
      if (remaining.isEmpty) openParens == 0
      else if (openParens == 0 && remaining.head == ')') false
      else if (remaining.head == '(') go(remaining.tail, openParens + 1)
      else go(remaining.tail, openParens - 1)
    }

    go(string, 0)
  }
  /*
    n => 1 => List("()")
    n => 2 => List("()()", "(())")
    n => 3 => List("()()()", "()(())", "((()))", "(()())")
   */
  // def generateAllValidParentheses(n: Int): List[String] = {
  //   /*
  //     () + () = prepend
  //     ( + () +) = inject
  //     () + () = append
  //    */
  // }

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def reverseWords(s: String): String = {
    val splitted = s.split(' ')
    def go(remaining: Array[String], acc: String): String = {
      if (remaining.isEmpty) acc
      else {
        val reversed = reverseChars(
          0,
          remaining.head.length - 1,
          StringBuilder(remaining.head)
        )
        if (remaining.tail.isEmpty) acc + reversed
        else go(remaining.tail, acc + reversed + ' ')
      }
    }

    @tailrec
    def reverseChars(
        firstIndex: Int,
        secondIndex: Int,
        acc: StringBuilder
    ): String = {
      if (firstIndex >= secondIndex) acc.toString
      else {
        val first = acc(firstIndex)
        val second = acc(secondIndex)
        acc(second) = first
        acc(first) = second

        reverseChars(firstIndex + 1, secondIndex - 1, acc)
      }
    }
    go(splitted, "")
  }

  println(reverseWords("Let's take LeetCode contest"))
}
