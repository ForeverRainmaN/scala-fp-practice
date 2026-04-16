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

  println(hasValidParentheses("()")) // true
  println(hasValidParentheses("()()")) // true
  println(hasValidParentheses("[]")) // true
  println(hasValidParentheses("(())")) // true
  println(hasValidParentheses("{}")) // true
  println(hasValidParentheses("(()")) // false
  println(hasValidParentheses("(()}")) // false
  println(hasValidParentheses("(()]")) // false
  println(hasValidParentheses(")(")) // false
  println(hasValidParentheses("][")) // false
  println(hasValidParentheses("}{")) // false
}
