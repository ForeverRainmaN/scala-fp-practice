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

  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]

  def rle: RList[(T, Int)]

  def duplicateEach(k: Int): RList[T]

  def rotate(k: Int): RList[T]

  def sample(k: Int): RList[T]

  def sorted[S >: T](ordering: Ordering[S]): RList[S]
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

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[Nothing] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(k: Int): RList[Nothing] = RNil

  override def rotate(k: Int): RList[Nothing] = RNil

  override def sample(k: Int): RList[Nothing] = RNil

  override def sorted[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
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

  // def map[S](f: T => S): RList[S] = f(head) :: tail.map(f)
  // Complexity: O(N)
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def go(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else go(remaining.tail, f(remaining.head) :: acc)
    }

    go(this, RNil)
  }

  // Complexity:
  // Naive: O(N)
  // Less naive: O(sum of all the lengths of f(x) = Z)
  // Real: O(z^2)
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def go(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else go(remaining.tail, f(remaining.head).reverse ++ acc)
    }

    /*
      [1,2,3].flatMap(x => [x, x * 2]) = betterFlatMap([1,2,3], [])
      = betterFlatMap([2,3], [[2,1]]
      = betterFlatMap([3], [[4,2],[2,1]])
      = betterFlatMap([], [[6,3]], [4,2], [2,1]])
      = concatenateAll([[6,3], [4,2], [2,1]], [], [])
      = concatenateAll([[4,2],[2,1], [6,3], [])
      = concatenateAll([[4,2],[2,1], [3], [6])
      = concatenateAll([[4,2],[2,1], [], [3,6])
      etc
     */
    // Complexity: O(N + Z)
    @tailrec
    def betterFlatMap(remaining: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty) concatenateAll(accumulator, RNil, RNil)
      else betterFlatMap(remaining.tail, f(remaining.head).reverse :: accumulator)
    }

    // Complexity: O(Z)
    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], accumulator: RList[S]): RList[S] =
      if (currentList.isEmpty && elements.isEmpty) accumulator
      else if (currentList.isEmpty) concatenateAll(elements.tail, elements.head, accumulator)
      else concatenateAll(elements, currentList.tail, currentList.head :: accumulator)

    betterFlatMap(this, RNil)
  }

  // Complexity: O(N)
  override def filter(predicate: T => Boolean): RList[T] = {
    @tailrec
    def go(remaining: RList[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc.reverse
      else if (predicate(remaining.head)) go(remaining.tail, remaining.head :: acc)
      else go(remaining.tail, acc)
    }

    go(this, RNil)
  }

  // Complexity: O(N)

  override def rle: RList[(T, Int)] = {
    @tailrec
    def go(remaining: RList[T], acc: RList[(T, Int)], count: Int): RList[(T, Int)] = {
      if (remaining.isEmpty) acc
      else if (remaining.tail.isEmpty) (remaining.head, count) :: acc
      else if (remaining.head != remaining.tail.head) go(remaining.tail, (remaining.head, count) :: acc, count = 1)
      else go(remaining.tail, acc, count + 1)
    }

    go(this, RNil, 1).reverse
  }

  def rle_v2: RList[(T, Int)] = {
    @tailrec
    def go(remaining: RList[T], currentTuple: (T, Int), acc: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty && currentTuple._2 == 0) acc
      else if (remaining.isEmpty) currentTuple :: acc
      else if (remaining.head == currentTuple._1) go(remaining.tail, currentTuple.copy(_2 = currentTuple._2 + 1), acc)
      else go(remaining.tail, (remaining.head, 1), currentTuple :: acc)
    }

    go(this.tail, (this.head, 1), RNil).reverse
  }

  override def duplicateEach(k: Int): RList[T] = {
    @tailrec
    def go(remaining: RList[T], acc: RList[T], currentElement: T, currElementCount: Int): RList[T] = {
      if (remaining.isEmpty && currElementCount == 0) acc
      else if (remaining.isEmpty && currElementCount == k) currentElement :: acc
      else if (currElementCount < k) go(remaining, currentElement :: acc, currentElement, currElementCount + 1)
      else go(remaining.tail, acc, remaining.head, 0)
    }

    if (k <= 0) this
    else go(this.tail, RNil, this.head, 0).reverse
  }

  def duplicateEach_v2(k: Int): RList[T] = {
    @tailrec
    def constructLists(currentLength: Int, currentElement: T, acc: RList[T]): RList[T] = {
      if (currentLength == k) acc
      else constructLists(currentLength + 1, currentElement, currentElement :: acc)
    }

    if (k <= 0) this
    else this.flatMap(x => constructLists(0, x, RNil))
  }

  def duplicateEach_v3(k: Int): RList[T] = {
    if (k <= 0) this
    else this.flatMap { elem =>
      @tailrec
      def replicate(n: Int, acc: RList[T]): RList[T] =
        if (n == 0) acc
        else replicate(n - 1, elem :: acc)

      replicate(k, RNil)
    }
  }

  // Complexity: O(N * K)
  def duplicateEach_v4(k: Int): RList[T] = {
    @tailrec
    def go(remaining: RList[T], currentElement: T, nDuplications: Int, accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty && nDuplications == k) accumulator.reverse
      else if (remaining.isEmpty) go(remaining, currentElement, nDuplications + 1, currentElement :: accumulator)
      else if (nDuplications == k) go(remaining.tail, remaining.head, 0, accumulator)
      else go(remaining, currentElement, nDuplications + 1, currentElement :: accumulator)
    }

    go(this.tail, this.head, 0, RNil)
  }

  override def rotate(k: Int): RList[T] = {
    val n = length
    if (n == 0) RNil
    else {
      val effectiveK = ((k % n) + n) % n
      if (effectiveK == 0) this
      else {
        @tailrec
        def go(remaining: RList[T], accumulator: RList[T], count: Int): RList[T] = {
          if (count == effectiveK) remaining ++ accumulator.reverse
          else go(remaining.tail, remaining.head :: accumulator, count + 1)
        }

        go(this, RNil, 0)
      }
    }
  }

  // Complexity: O(max(N, K))

  def rotate_v2(k: Int): RList[T] = {
    @tailrec
    def go(remaining: RList[T], rotationsLeft: Int, acc: RList[T]): RList[T] = {
      if (remaining.isEmpty && rotationsLeft == 0) this
      else if (remaining.isEmpty) go(this, rotationsLeft, RNil)
      else if (rotationsLeft == 0) remaining ++ acc.reverse
      else go(remaining.tail, rotationsLeft - 1, remaining.head :: acc)
    }

    go(this, k, RNil)
  }

  override def sample(k: Int): RList[T] = {
    if (k <= 0 || this.isEmpty) RNil
    else {
      import scala.util.Random

      @tailrec
      def convertToVector(remaining: RList[T], acc: Vector[T]): Vector[T] = {
        if (remaining.isEmpty) acc
        else convertToVector(remaining.tail, remaining.head +: acc)
      }

      val vec = convertToVector(this, Vector.empty[T])
      val vecLength = vec.length

      @tailrec
      def go(resultList: RList[T], count: Int): RList[T] = {
        if (count == k) resultList
        else go(vec(Random.nextInt(vecLength)) :: resultList, count + 1)
      }

      go(RNil, 0)
    }
  }

  // Complexity: O(N * K)
  def sample_v2(k: Int): RList[T] = {
    import scala.util.Random
    val random = new Random

    @tailrec
    def go(list: RList[T], accumulator: RList[T], randomIndex: Int): RList[T] = {
      if (accumulator.length == k) accumulator
      else go(list, list.apply(randomIndex) :: accumulator, random.nextInt(length))
    }

    go(this, RNil, random.nextInt(length))
  }

  def sample_v3(k: Int): RList[T] = {
    import scala.util.Random
    val random = new Random

    @tailrec
    def go(list: RList[T], accumulator: RList[T], randomIndex: Int, count: Int): RList[T] = {
      if (accumulator.length == k) accumulator
      else if (count < randomIndex) go(list.tail, accumulator, randomIndex, count + 1)
      else go(this, list.head :: accumulator, random.nextInt(length), count = 0)
    }

    go(this, RNil, random.nextInt(length), 0)
  }

  def sample_v4(k: Int): RList[T] = {
    import scala.util.Random

    val random = new Random(System.currentTimeMillis())
    val maxIndex = this.length

    @tailrec
    def go(nRemaining: Int, accumulator: RList[T]): RList[T] = {
      if (nRemaining == 0) accumulator
      else {
        val index = random.nextInt(maxIndex)
        val newElement = this (index)
        go(nRemaining - 1, newElement :: accumulator)
      }
    }

    // Complexity: O(N * K)
    def goElegant: RList[T] = RList.from((1 to k).map(_ => random.nextInt(maxIndex)).map(index => this (index)))

    if (k < 0) RNil
    else go(k, RNil)
  }

  override def sorted[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
         insertSorted(4, [], [1,2,3,5]) =
         insertSorted(4, [1], [2,3,5]) =
         insertSorted(4, [2,1], [3,5]) =
         insertSorted(4, [3,2,1], [5]) =
         [3,2,1].reverse + (4 :: [5]) =
         [1,2,3,4,5]
        */

    // Complexity: O(N)
    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S]): RList[S] = {
      if (after.isEmpty || ordering.lteq(element, after.head)) before.reverse ++ (element :: after)
      else insertSorted(element, after.head :: before, after.tail)
    }

    // Complexity: O(N^2)
    @tailrec
    def insertSortTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else insertSortTailrec(remaining.tail, insertSorted(remaining.head, RNil, acc))
    }

    insertSortTailrec(this, RNil)
  }
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
  println(RList.from(List(1, 2, 3, 4, 5, 6, 7, 8, 9)).sample(10))
  println(RList.from(List(1, 2, 3, 4, 5)).sorted((a, b) => if (a > b) 1 else 0))
}
