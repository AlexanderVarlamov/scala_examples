package main



object longestSubstring extends App {
  val st1 = "abcabcbb"
  val st2 = "bbbbb"
  val st3 = "jxdlnaaij"
  val st4 = "pwwkew"


  def lengthOfLongestSubstring(s: String): Int = {
    val chArr = s.toCharArray
    import scala.annotation.tailrec


    @tailrec
    def helperFunc(chars: Array[Char],
                   positions: Map[Char, Int] = Map[Char, Int](),
                   maxL:Int=0,
                   lastUniquePos: Int = 0,
                   pos: Int=0): Int = {
      if (chars.isEmpty) maxL
      else {
        val currentSymbol = chars.head
        positions.get(currentSymbol) match {
          case Some(previousValue) =>
            val newLastUniquePosition = lastUniquePos.max(previousValue)
            val newMaxL = (pos-newLastUniquePosition+1).max(maxL)
            helperFunc(chars.tail, positions+(currentSymbol -> (pos+1)), newMaxL, newLastUniquePosition, pos+1)
         case None => helperFunc(chars.tail, positions+(currentSymbol -> (pos+1)), maxL.max(pos-lastUniquePos+1), lastUniquePos, pos+1)
        }
      }
    }
    helperFunc(chArr)
  }


  Array(
//    st1,
//    st2,
    st3,
    st4) foreach (x => println(lengthOfLongestSubstring(x)))
}
