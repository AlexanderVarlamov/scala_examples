package main

import scala.collection.mutable

object two_sum extends App {
  val nums = Array(3, 2, 4)
  val target = 6

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val length = nums.length
    val indexes = mutable.Map[Int, Int]()
    var i = 0
    var result = Array[Int]()
    while (i < length) {
      val current = nums(i)
      val diff = target - current
      indexes.get(diff) match {
        case Some(value) => result = Array(value, i); i = length
        case None => indexes(current) = i; i += 1
      }
    }
    result
  }

  println(twoSum(nums, target).mkString("Array(", ", ", ")"))


}
