package com.rockthejvm.lists

sealed abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean
}

case object RNil extends RList[Nothing] {

  override def head: Nothing = throw NoSuchElementException

  override def tail: RList[Nothing] = throw NoSuchElementException

  override def isEmpty: Boolean = true
}

case class Cons[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
}

object ListProblems extends App {

}
