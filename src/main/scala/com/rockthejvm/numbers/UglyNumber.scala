package com.rockthejvm.numbers
import scala.annotation.tailrec
import scala.collection.mutable.Queue

object UglyNumber {

  // ugly = ony the factors 2, 3, 5
  // 1 is ugly
  // assume positive inputs
  // example 6, 25, 100
  // not ugly: 14, 39

  def uglyNumber(number: Int): Boolean = {
    @tailrec
    def go(remainder: Int): Boolean = {
      if (remainder == 1) true
      else {
        if (remainder % 2 == 0) go(remainder / 2)
        else if (remainder % 3 == 0) go(remainder / 3)
        else if (remainder % 5 == 0) go(remainder / 5)
        else false
      }
    }

    go(number)
  }
  // the nth ugly number, given the index
  def nthUgly(index: Int): Int = {
    @tailrec
    def go(uglyCount: Int, currentUgly: Int, maybeUgly: Int): Int = {
      if (uglyCount == index) currentUgly
      else if (uglyNumber(maybeUgly))
        go(uglyCount + 1, maybeUgly, maybeUgly + 1)
      else go(uglyCount, currentUgly, maybeUgly + 1)
    }

    go(0, 1, 2)
  }

  def nthUglyV2(index: Int): Int = {
    @tailrec
    def go(
        uglyCount: Int,
        currentUgly: Int,
        x2Queue: Queue[Int],
        x3Queue: Queue[Int],
        x5Queue: Queue[Int]
    ): Int = {
      if (uglyCount == index) currentUgly
      else {
        x2Queue.enqueue(currentUgly * 2)
        x3Queue.enqueue(currentUgly * 3)
        x5Queue.enqueue(currentUgly * 5)

        val smallestUgly =
          Math.min(x2Queue.min, Math.min(x3Queue.min, x5Queue.min))

        if (x2Queue.contains(smallestUgly)) {
          x2Queue.dequeue()
        }

        if (x3Queue.contains(smallestUgly)) {
          x3Queue.dequeue()
        }

        if (x5Queue.contains(smallestUgly)) {
          x5Queue.dequeue()
        }

        go(uglyCount + 1, smallestUgly, x2Queue, x3Queue, x5Queue)
      }
    }

    go(1, 1, Queue.empty, Queue.empty, Queue.empty)
  }

  def nthUglyV3(n: Int): Int = {
    def min3(a: Int, b: Int, c: Int): Int =
      if (a <= b)
        if (a <= c) a else c
      else if (b <= c) b
      else c

    @tailrec
    def nthUglyTailrec(
        index: Int,
        q2: Queue[Int],
        q3: Queue[Int],
        q5: Queue[Int]
    ): Int = {
      val min = min3(q2.head, q3.head, q5.head)
      if (index == n) min
      else {
        val newQ2 = (if (min == q2.head) q2.tail else q2).enqueue(min * 2)
        val newQ3 = (if (min == q3.head) q3.tail else q3).enqueue(min * 3)
        val newQ5 = (if (min == q5.head) q5.tail else q5).enqueue(min * 5)

        nthUglyTailrec(index + 1, newQ2, newQ3, newQ5)
      }
    }

    if (n == 1) 1
    else nthUglyTailrec(2, Queue(2), Queue(3), Queue(5))
  }

  def main(args: Array[String]): Unit = {
    println(nthUglyV2(7))
  }
}
