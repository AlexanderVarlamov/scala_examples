package main

import scala.util.Random


object Sorting extends App {

  import scala.annotation.tailrec

//
//  val n = scala.io.StdIn.readLine().toInt
//  val arr = scala.io.StdIn.readLine().split(" ").map(BigInt(_)).toList


  def mergeList(A: List[BigInt]): (List[BigInt], Int) = {
    if (A.isEmpty || A.length == 1) (A, 0)
    else {
      val (leftUnsorted, rightUnsorted) = A.splitAt(A.length / 2)
      val left = mergeList(leftUnsorted)
      val right = mergeList(rightUnsorted)
      mergeSortRec(left._1, right._1, List.empty[BigInt], left._2 + right._2)
    }
  }

  @tailrec
  def mergeSortRec(leftList: List[BigInt], rightList: List[BigInt], acc: List[BigInt] = List(), accC: Int = 0): (List[BigInt], Int) = {
    if (leftList.isEmpty) (acc ++ rightList, accC)
    else if (rightList.isEmpty) (acc ++ leftList, accC)
    else {
      val leftHead = leftList.head
      val rightHead = rightList.head
      if (leftHead <= rightHead) mergeSortRec(leftList.tail, rightList, acc :+ leftHead, accC)
      else {
        mergeSortRec(leftList, rightList.tail, acc :+ rightHead, accC + leftList.length)
      }
    }
  }

  def quickSort(unsorted:Array[Int]): Array[Int] = {
    if (unsorted.length <= 1) unsorted
    else {
      val point = unsorted(Random.nextInt(unsorted.length))
      val (lessThen, moreOrEqual) = unsorted.partition( _ < point)
      val (equal, moreThan) = moreOrEqual.partition( _ == point)
      quickSort(lessThen) ++ equal ++ quickSort(moreThan)
    }
  }

  val testArr = Array.fill(10)(Random.nextInt(1000))
  println(quickSort(testArr).mkString("Array(", ", ", ")"))
//  val listSorted = mergeList(arr)
//  println(listSorted._2)

}
