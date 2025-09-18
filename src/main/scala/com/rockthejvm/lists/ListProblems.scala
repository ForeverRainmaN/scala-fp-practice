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

  def reverse: RList[T]

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]
}

case object RNil extends RList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil
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

  /*
    [1,2,3,4].reverse = go([1,2,3,4], RNil)
    = go([2,3,4], [1])
    = go([3,4], [2,1])
    = go([4], [3,2,1])
    = go([], [4,3,2,1])
    = [4,3,2,1]

    Complexity: O(n)
   */

  override def reverse: RList[T] = {
    @tailrec
    def go(remaining: RList[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else go(remaining.tail, remaining.head :: acc)
    }

    go(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def go(remaining: RList[S], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else go(remaining.tail, remaining.head :: acc)
    }

    go(anotherList, this.reverse).reverse
    //def ++[S >: T](anotherList: RList[S]): RList[S] = head :: (tail ++ anotherList)
  }

  // Complexity: O(N)
  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def go(count: Int, leftSide: RList[T], rightSide: RList[T]): RList[T] = {
      if (count == index) leftSide.reverse ++ rightSide.tail
      else if (rightSide.isEmpty) leftSide.reverse
      else go(count + 1, rightSide.head :: leftSide, rightSide.tail)
    }

    go(0, RNil, this)
  }
  /*
    def removeAt(index: Int): RList[T] =
        if(index == 0) tail
        else head :: tail.removeAt(index-1)
   */
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def convertToRlist(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRlist(remaining.tail, remaining.head :: acc)
    }

    convertToRlist(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  val aSmallList = 1 :: 2 :: 3 :: RNil
  val anotherList = 4 :: 5 :: 6 :: RNil
  val newList = aSmallList.removeAt(6)
  println(newList)
}
