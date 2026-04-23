package com.rockthejvm.strings

import scala.annotation.tailrec
import scala.collection.mutable.Map as MutableMap

object RansomNote extends App {
  def ransomNote(note: String, magazine: String): Boolean = {
    @tailrec
    def createFreqMap(
        s: String,
        freqMap: MutableMap[Char, Int]
    ): MutableMap[Char, Int] = {
      if (s.isEmpty) freqMap
      else {
        val c = s.head
        freqMap(c) = freqMap.getOrElse(c, 0) + 1
        createFreqMap(s.tail, freqMap)
      }
    }

    val freqMap = createFreqMap(magazine, MutableMap.empty)

    @tailrec
    def go(note: String, freqMap: MutableMap[Char, Int]): Boolean = {
      if (note.isEmpty) true
      else if (note.head == ' ') go(note.tail, freqMap)
      else {
        val letter = note.head
        if (!freqMap.contains(letter)) false
        else {
          if (freqMap(letter) == 1) {
            freqMap.remove(letter)
            go(note.tail, freqMap)
          } else {
            freqMap(letter) = freqMap(letter) - 1
            go(note.tail, freqMap)
          }
        }
      }
    }

    go(note, freqMap)

  }

  def ransomNoteV2(note: String, magazine: String): Boolean = {
    def buildMap(string: String): Map[Char, Int] =
      string.foldLeft(Map[Char, Int]()) { case (map, char) =>
        map + (char -> (map.getOrElse(char, 0) + 1))
      }

    val noteMap = buildMap(note)
    val magazineMap = buildMap(magazine)

    noteMap.keySet.forall(char =>
      noteMap.getOrElse(char, 0) <= magazineMap.getOrElse(char, 0)
    )
  }

  def ransomNoteV3(note: String, magazine: String): Boolean = {
    val noteMap = note.groupBy(identity).mapValues(_.length).toMap
    val magazineMap = magazine.groupBy(identity).mapValues(_.length).toMap

    noteMap.forall { case (char, count) =>
      count <= magazineMap.getOrElse(char, 0)
    }
  }
}
