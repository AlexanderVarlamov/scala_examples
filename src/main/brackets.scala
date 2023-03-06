package main

object brackets extends App {

  import scala.annotation.tailrec

  //  val st = scala.io.StdIn.readLine()
  val st = "foo(bar[i]);"

  @tailrec
  def check_brackets(stringToCheck: String, stack: List[(Char, Int)] = Nil, position: Int = 0): String = {
    if (stringToCheck.isEmpty) {
      stack match {
        case Nil => "Success"
        case _ => stack.head._2.toString
      }
    }
    else {
      val possibleError = (position + 1).toString
      val charToCheck = stringToCheck(0)
      if (('{' :: '[' :: '(' :: Nil).contains(charToCheck)) check_brackets(stringToCheck.drop(1), (charToCheck, position + 1) +: stack, position + 1)
      else if (('}' :: ')' :: ']' :: Nil).contains(charToCheck)) {
        if (stack.isEmpty) possibleError
        else {
          charToCheck match {
            case '}' => if (stack.head._1 == '{') check_brackets(stringToCheck.drop(1), stack.tail, position + 1) else possibleError
            case ']' => if (stack.head._1 == '[') check_brackets(stringToCheck.drop(1), stack.tail, position + 1) else possibleError
            case ')' => if (stack.head._1 == '(') check_brackets(stringToCheck.drop(1), stack.tail, position + 1) else possibleError
          }
        }
      }
      else check_brackets(stringToCheck.drop(1), stack, position + 1)
    }
  }
  val result = check_brackets(st)
  println(result)
}
