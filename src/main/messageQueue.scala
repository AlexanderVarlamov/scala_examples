package main

import scala.annotation.tailrec
import scala.collection.mutable.Queue

object messageQueue extends App {
  val firstString = scala.io.StdIn.readLine()

  val intArr = firstString.split(" ").map(_.toInt)
  var bufferSize = intArr(0)
  val numbOfPack = intArr(1)
  var queue = Queue[(msg, Int)]()
  var procTime = 0
  val answers = Array.fill(numbOfPack)(-3)

  for (msgNum <- 0 until numbOfPack) {
    val rawSt = scala.io.StdIn.readLine().split(" ").map(_.toInt)
    val arrival = rawSt(0)
    val dur = rawSt(1)
    val currentMsg = msg(arrival, dur)

    if (arrival < procTime) answers(msgNum) = -1 else {
      processQueue(queue, arrival, procTime, bufferSize) match {
        case (qq, pp, bs) => queue = qq; procTime = pp; bufferSize = bs
      }
      if (bufferSize <= 0) answers(msgNum) = -1 else {
        queue.enqueue((currentMsg, msgNum))
        bufferSize -= 1
      }
    }
  }
  //  println(s"size of queue: ${queue.size}")
  processQueue(queue, Int.MaxValue, procTime, bufferSize)
  answers foreach println


  @tailrec
  def processQueue(q: Queue[(msg, Int)], curTime: Int, procTime: Int, bufSize: Int): (Queue[(msg, Int)], Int, Int) = {
    if (q.isEmpty) (q, curTime, bufSize)
    else {
      val oldQueue = q.clone()
      val (currMsg, number) = q.dequeue()
      val msgTime = currMsg.arrival
      msgTime match {
        case _ if curTime >= procTime+currMsg.duration =>
          val newProcTime = currMsg.duration + procTime
          answers(number) = procTime
          processQueue(q, curTime, newProcTime, bufSize+1)
        case _ => (oldQueue, procTime, bufSize)
      }
    }
  }


  case class msg(arrival: Int, duration: Int)
}