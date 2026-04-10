package com.rockthejvm.numbers
import scala.annotation.tailrec

object Duplicates extends App {
  // all numbers in the list appear exactly TWICE, except one: find that number
  def duplicates(list: List[Int]): Int =
    list
      .groupBy(identity)
      .collectFirst { case (num, count :: Nil) => num }
      .get

  def duplicatesv2(list: List[Int]): Int = {
    @tailrec
    def createNumToCountMap(
        remaining: List[Int],
        cache: Map[Int, Int]
    ): Map[Int, Int] = {
      if (remaining.isEmpty) cache
      else if (cache.contains(remaining.head)) {
        val newCache = cache.updated(remaining.head, cache(remaining.head) + 1)
        createNumToCountMap(remaining.tail, newCache)
      } else {
        val newCache = cache + (remaining.head -> 1)
        createNumToCountMap(remaining.tail, newCache)
      }
    }

    val numToCount = createNumToCountMap(list, Map.empty)

    numToCount.find(_._2 == 1).get._1
  }
  // complexity O(n) time, O(n) space with some optimization, at most n/2 elements in the set
  def duplicatesv3(list: List[Int]): Int = {
    @tailrec
    def go(remaining: List[Int], acc: Set[Int]): Int = {
      if (remaining.isEmpty) acc.head
      else if (acc(remaining.head)) {
        go(remaining.tail, acc - remaining.head)
      } else go(remaining.tail, acc + remaining.head)
    }
    if (list.isEmpty)
      throw new IllegalArgumentException(
        "list doesn't contain that unique number"
      )
    else go(list, Set.empty)
  }
// complexity: O(n^2)
  def duplicatesNaive(list: List[Int]): Int = {
    @tailrec
    def go(remainder: List[Int]): Int = {
      if (remainder.isEmpty)
        throw new IllegalArgumentException(
          "list doesn't contain that unique number"
        )
      else {
        val element = remainder.head
        val elementCount = list.count(_ == element)

        if (elementCount == 1) element
        else go(remainder.tail)
      }
    }

    go(list)
  }
  // O(N) time, O(n) space
  @tailrec
  def naiveWithMemory(
      remainder: List[Int],
      occurrences: Map[Int, Int] = Map()
  ): Int =
    if (remainder.isEmpty) occurrences.filter(_._2 == 1).head._1
    else {
      val currentNumber = remainder.head
      val currentOccurences = occurrences.getOrElse(currentNumber, 0)
      naiveWithMemory(
        remainder.tail,
        occurrences + (currentNumber -> (currentOccurences + 1))
      )
    }

  // optimal
  // ^ = XOR
  // 0 ^ 0 = 0
  // 1 ^ 1 = 0
  // val optimal = list.foldLeft(0)(_ ^ _)

  println(duplicatesv3(List(1, 1, 2, 6, 2, 3, 3)))
}
