package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
        println()
    }
  }
  /**
   * p(a, b) = p(a - 1, b - 1) + p(a, b - 1)
   *
   */

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c == 0) || (c == r)) return 1
    return pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var qCount = 0
    var leftQCount = 0
    var rightQCount = 0
    chars.foreach(c => {
      c match {
        case '(' => {
          qCount += 1
          leftQCount += 1
        }
        case ')' => {
          rightQCount += 1
          if (leftQCount > 0) {
            qCount -= 1
            leftQCount -= 1
            rightQCount -= 1
          }
        }
        case _ => None
      }
    })
    return (qCount == 0) && (rightQCount == 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sorted = coins.sorted
    def countRecursive(m: Int, coins: List[Int]): Int = {
      if (coins.length == 0) {
        return 0
      }
      val smallest = coins.head
      if (smallest > m) return 0
      if (smallest == m) {
        return 1
      }

      return countRecursive(m - smallest, coins) + countRecursive(m, coins.tail)
    }
    return countRecursive(money, sorted)
  }
}
