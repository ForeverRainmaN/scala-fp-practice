package com.rockthejvm.trees
import scala.annotation.tailrec

enum BTree[+T]:
  case Empty
  case BNode(value: T, left: BTree[T], right: BTree[T])

  def maybeValue: Option[T] = this match
    case Empty          => None
    case BNode(v, _, _) => Some(v)

  def maybeLeft: Option[BTree[T]] = this match
    case BNode(_, l, _) => Some(l)
    case Empty          => None

  def maybeRight: Option[BTree[T]] = this match
    case BNode(_, _, r) => Some(r)
    case Empty          => None

  def isEmpty: Boolean = this match
    case Empty => true
    case _     => false

  def isLeaf: Boolean = this match
    case BNode(_, Empty, Empty) => true
    case _                      => false

  def collectLeaves: List[T] = {
    @tailrec
    def go(
        stack: List[BTree[T]],
        acc: List[T]
    ): List[T] = stack match {
      case Nil                            => acc
      case Empty :: rest                  => go(rest, acc)
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
      case Nil                            => totalCount
      case Empty :: rest                  => go(rest, totalCount)
      case BNode(v, Empty, Empty) :: rest => go(rest, totalCount + 1)
      case BNode(_, l, r) :: rest         => go(l :: r :: rest, totalCount)

    go(List(this), 0)
  }

object BinaryTreeProblems extends App {}
