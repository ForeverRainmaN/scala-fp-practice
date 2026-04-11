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

  // O(n + m) time O(1) space
  def checkAnagramsV1(sa: String, sb: String): Boolean = {
    def createArrayFromString(s: String): Array[Int] = {
      val array = Array.ofDim[Int](26)

      for (ch <- s) {
        val index = ch - 'a'
        array(index) += 1
      }

      array
    }

    val firstArray = createArrayFromString(sa.toLowerCase)
    val secondArray = createArrayFromString(sb.toLowerCase)

    firstArray sameElements secondArray
  }
  // O(n + m) time O(k), where k = unique symbols
  def checkAnagramsV2(sa: String, sb: String): Boolean = {
    @tailrec
    def createMap(str: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (str.isEmpty) acc
      else {
        val char = str.head
        val newCount = acc.getOrElse(char, 0) + 1

        createMap(str.tail, acc.updated(char, newCount))
      }
    }

    val mapCharToCount = createMap(sa.toLowerCase, Map.empty)

    @tailrec
    def go(remaining: String, cache: Map[Char, Int]): Boolean = {
      if (remaining.isEmpty && cache.isEmpty) true
      else {
        val char = remaining.head
        if (!cache.contains(char)) false
        else if (cache(char) == 1) go(remaining.tail, cache - char)
        else go(remaining.tail, cache.updated(char, cache(char) - 1))
      }
    }

    go(sb.toLowerCase, mapCharToCount)
  }

  def checkAnagramsV3(sa: String, sb: String): Boolean =
    countCharactersV1(sa) == countCharactersV1(sb)

  def checkAnagramsV4(sa: String, sb: String): Boolean =
    sa.sorted == sb.sorted

  println(checkAnagramsV2("desserts", "stressed"))
  println(checkAnagramsV2("Scala", "Haskell"))
}
