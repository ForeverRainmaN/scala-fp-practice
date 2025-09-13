package com.rockthejvm.lists

import scala.annotation.{tailrec, targetName}

sealed abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  @targetName("prepend")
  def ::[S >: T](elem: S): RList[S] = new::(elem, this)

  def apply(index: Int): T

  def length: Int
}

case object RNil extends RList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  // Complexity O(min(n, index))
  override def apply(index: Int): T = {
    @tailrec
    def go(i: Int, current: RList[T]): T =
      if (i == index) current.head
      else go(i + 1, current.tail)

    if (index < 0) throw new NoSuchElementException
    else go(0, this)
  }

  override def length: Int = {
    @tailrec
    def go(size: Int, remaining: RList[T]): Int =
      if (remaining.isEmpty) size
      else go(size + 1, remaining.tail)

    go(0, this)
  }
}

object ListProblems extends App {
  val aSmallList = 1 :: 2 :: 3 :: RNil
  println(aSmallList.length)
}
