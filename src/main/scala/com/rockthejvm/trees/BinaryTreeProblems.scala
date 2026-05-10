package com.rockthejvm.trees

import scala.annotation.tailrec

enum BTree[+T]:
  case Empty
  case BNode(value: T, left: BTree[T], right: BTree[T])

  def maybeValue: Option[T] = this match
    case Empty => None
    case BNode(v, _, _) => Some(v)

  def maybeLeft: Option[BTree[T]] = this match
    case BNode(_, l, _) => Some(l)
    case Empty => None

  def maybeRight: Option[BTree[T]] = this match
    case BNode(_, _, r) => Some(r)
    case Empty => None

  def isEmpty: Boolean = this match
    case Empty => true
    case _ => false

  def isLeaf: Boolean = this match
    case BNode(_, Empty, Empty) => true
    case _ => false

  def size: Int =
    @tailrec
    def go(stack: List[BTree[T]], size: Int): Int = stack match
      case Nil => size
      case Empty :: rest => go(rest, size)
      case BNode(_, l, r) :: rest => go(l :: r :: rest, size + 1)

    go(List(this), 0)

  def collectLeaves: List[T] = {
    @tailrec
    def go(
            stack: List[BTree[T]],
            acc: List[T]
          ): List[T] = stack match {
      case Nil => acc
      case Empty :: rest => go(rest, acc)
      case BNode(v, Empty, Empty) :: rest =>
        go(rest, v :: acc)
      case BNode(_, l, r) :: rest =>
        go(l :: r :: rest, acc)
    }

    go(List(this), List.empty)
  }

  def leafCount: Int = {
    @tailrec
    def go(stack: List[BTree[T]], totalCount: Int): Int = stack match
      case Nil => totalCount
      case Empty :: rest => go(rest, totalCount)
      case BNode(v, Empty, Empty) :: rest => go(rest, totalCount + 1)
      case BNode(_, l, r) :: rest => go(l :: r :: rest, totalCount)

    go(List(this), 0)
  }

  def collectNodes(level: Int): List[BTree[T]] =
    @tailrec
    def go(currentLevel: Int, currentNodes: List[BTree[T]]): List[BTree[T]] =
      if (currentLevel == level) currentNodes
      else {
        val nextLevelNodes = currentNodes.flatMap {
          case BNode(_, l, r) => List(l, r)
          case Empty => Nil
        }
        go(currentLevel + 1, nextLevelNodes)
      }

    go(0, List.empty)


object BinaryTreeProblems extends App {

  import BTree.{BNode, Empty}

  val tree: BTree[Int] =
    BNode(1, BNode(2, Empty, Empty), BNode(3, Empty, Empty))
  /*
      1
     / \
    2   3
  /
  4
   */

  println(tree.size)
}
