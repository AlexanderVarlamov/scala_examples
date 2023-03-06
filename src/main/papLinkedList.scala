package main


object papLinkedList extends App {
  val listEx = ListNode(1, ListNode(2, ListNode(2, ListNode(1))))
  val l1 = ListNode(2, ListNode(4, ListNode(3)))
  val l2 = ListNode(5, ListNode(6, ListNode(4)))


  def isPalindrome(head: ListNode): Boolean = {
    import scala.annotation.tailrec
    @tailrec
    def toArray(listOfNodes: ListNode, accforward: String = ""): String = {
      listOfNodes.next match {
        case null => accforward + listOfNodes.x.toString
        case _ => toArray(listOfNodes.next, accforward + listOfNodes.x.toString)
      }
    }

    val word = toArray(head)
    word == word.reverse

  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    import scala.annotation.tailrec
    @tailrec
    def innerHelper(l1Looped: ListNode, l2Looped: ListNode, acc: ListNode = null, addOne: Int = 0): ListNode = {
      l1Looped match {
        case null => l2Looped match {
          case null => if (addOne == 0) acc else ListNode(addOne, acc)
          case _ =>
            val currentL2X = l2Looped.x
            val sumDigits = currentL2X + addOne
            val (digitToSend, nextAddOne) = {
              if (sumDigits <= 9) (sumDigits, 0)
              else (sumDigits - 10, 1)
            }
            innerHelper(l1Looped, l2Looped.next, ListNode(digitToSend, acc), nextAddOne)
        }

        case _ => l2Looped match {
          case null =>
            val currentL1X = l1Looped.x
            val sumDigits = currentL1X + addOne
            val (digitToSend, nextAddOne) = {
              if (sumDigits <= 9) (sumDigits, 0)
              else (sumDigits - 10, 1)
            }
            innerHelper(l1Looped.next, l2Looped, ListNode(digitToSend, acc), nextAddOne)
          case _ =>
            val currentL1X = l1Looped.x
            val currentL2X = l2Looped.x
            val sumDigits = currentL1X + addOne + currentL2X
            val (digitToSend, nextAddOne) = {
              if (sumDigits <= 9) (sumDigits, 0)
              else (sumDigits - 10, 1)
            }
            innerHelper(l1Looped.next, l2Looped.next, ListNode(digitToSend, acc), nextAddOne)
        }
      }
    }

    @tailrec
    def reverseList(initialList: ListNode, acc: ListNode=null):ListNode = {
      initialList match {
        case null => acc
        case _ => reverseList(initialList.next, ListNode(initialList.x, acc))
      }
    }
    reverseList(innerHelper(l1, l2))
  }

  case class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x

  }

  println(addTwoNumbers(l1, l2))
}
