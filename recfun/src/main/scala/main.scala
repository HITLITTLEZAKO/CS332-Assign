package recfun

object Main {
  def countChangeCount(target: Int, coins: List[Int]): Int = {
    countChange(target, coins).length
  }
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

    /**
     * Exercise 1
     */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

    /**
     * Exercise 2
     */
    def balance(chars: List[Char]): Boolean = {
      val brackets = chars.filter(c => c == '(' || c == ')')

      def check(brackets: List[Char]): Boolean = {
        if (brackets.isEmpty) true
        else {
          val index = brackets.indexOfSlice(List('(', ')'))
          if (index == -1) false
          else {
            val newBrackets = brackets.take(index) ++ brackets.drop(index + 2)
            check(newBrackets)
          }
        }
      }

      check(brackets)
    }

    /**
     * Exercise 3
     */
    // def countChange(money: Int, coins: List[Int]): Int = ???
    def countChange(target: Int, coins: List[Int]): List[List[Int]] = {
      val sortedCoins = coins.sorted(Ordering[Int].reverse)
/**
 *To avoid duplicate combinations occurring in different orders,
 * I ensure that coins are added in non-increasing order by filtering usableCoins
 * to do that,i found the easiest way is to have the coins sorted in descending order first
 * after which I can always filter the coins that are less than or equal to the minimum coin in the current expression easily
 */
      def helper(currentExpr: List[Int], remaining: Int): List[List[Int]] = {
        if (remaining == 0) List(currentExpr) // finds a valid combination
        else if (remaining < 0 || sortedCoins.isEmpty) Nil // overflow or no coins left
        else {
          val minInExpr = if (currentExpr.isEmpty) sortedCoins.head else currentExpr.min
          val usableCoins = sortedCoins.filter(_ <= minInExpr)

          usableCoins.flatMap { coin =>
            helper(currentExpr :+ coin, remaining - coin)
          }
        }
      }
      helper(Nil, target)
    }
  /** this function returns all combinations of coins that sum to target as a list of lists
   *however,I found later that the test code only needs the count of combinations
   *So I added a function countChangeCount to return the length of the list of lists because I really don't want to right this again
   */
}

