import scala.annotation.tailrec


object Implementations {

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      return 1
    }

    return pascal(c, r - 1) + pascal(c - 1, r - 1)
  }


  def balance(chars: List[Char]): Boolean = {
    val finalCount = chars.foldLeft(0) { (count, char) =>
      if (count < 0) {
        -1
      } else {
        char match {
          case '(' => count + 1
          case ')' => count - 1
          case _ => count
        }
      }
    }

    finalCount == 0
  }

  def recursiveBalance(chars: List[Char]): Boolean = {
    @tailrec
    def countBalance(chars: List[Char], currentCount: Int): Boolean = {
      if (currentCount < 0) {
        false
      }
      else if (chars.isEmpty) {
        currentCount == 0
      }
      else if (chars.head == '(') {
        countBalance(chars.tail, currentCount + 1)
      }
      else if (chars.head == ')') {
        countBalance(chars.tail, currentCount - 1)
      }
      else {
        countBalance(chars.tail, currentCount)
      }
    }

    countBalance(chars, 0)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) {
      0
    }
    else if (money == 0) {
      1
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}