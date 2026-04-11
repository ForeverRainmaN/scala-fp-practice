package com.rockthejvm.strings
import scala.annotation.tailrec

object StringProblems extends App {

  def countCharactersV1(s: String): Map[Char, Int] = {
    @tailrec
    def go(remaining: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (remaining.isEmpty) acc
      else {
        val char = remaining.head
        val newCount = acc.getOrElse(char, 0) + 1
        go(remaining.tail, acc.updated(char, newCount))
      }
    }
    go(s, Map.empty)
  }

  def countCharactersV2(s: String): Map[Char, Int] =
    s.groupBy(identity).map { case (k, v) => (k, v.length) }
}
