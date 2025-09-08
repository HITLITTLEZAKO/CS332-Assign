object Main {
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("\nBracket Matching Tests:")
    testBalance()

    println("\nCount Change Tests:")
    testCountChange()
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

  // 测试函数
  def testBalance(): Unit = {//ai-generated test cases,i think it would be fine to use ai to generate test cases,,,,,,,,
    val testCases = List(
      ("(if (zero? x) max (/ 1 x))", true),
      ("I told him (that it's not (yet) done). (But he wasn't listening)", true),
      (":-)", false),
      ("())(", false),
      ("(((())))", true),
      ("(()", false),
      ("())", false),
      ("", true),
      ("no brackets here", true),
      ("(())(()())", true),
      ("(()))(()", false)
    )

    testCases.foreach { case (input, expected) =>
      val result = balance(input.toList)
      val status = if (result == expected) "✅ Passed" else "❌ Failed"
      println(s"$status | Input: '$input' | Expected: $expected | Got: $result")
    }
  }

  /**
   * Exercise 3
   */
  // def countChange(money: Int, coins: List[Int]): Int = ???
  def countChange(target: Int, coins: List[Int]): List[List[Int]] = {
    val sortedCoins = coins.sorted(Ordering[Int].reverse)
    def helper(currentExpr: List[Int], remaining: Int): List[List[Int]] = {
      if (remaining == 0) List(currentExpr) // 找到一个合法组合
      else if (remaining < 0 || sortedCoins.isEmpty) Nil // 超过目标或没有硬币可用
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

  def testCountChange(): Unit = {
    println("\nCount Change Tests:")

    val testCases = List(
      (4, List(1, 2), 3), // 所有组合：[1,1,1,1], [1,1,2], [2,2], [1,2,1], [2,1,1]
      (4, List(2, 1), 3),
      (5, List(1, 2, 5), 4),
      (5, List(5, 2, 1), 4),
      (3, List(2), 0),
      (10, List(2, 5, 3, 6), 5),
      (10, List(6, 5, 3, 2), 5),
      (0, List(1, 2, 3), 1),
      (0, List(3, 2, 1), 1),
      (1, List(), 0),
      (7, List(3, 2, 1), 8),
      (300, List(500,200,100,50,20,10,5), 1022) // 已知结果
      // 所有组合的数量
    )

    testCases.foreach { case (money, coins, expectedCount) =>
      val result = countChange(money, coins)
      val actualCount = result.length
      val status = if (actualCount == expectedCount) "✅ Passed" else "❌ Failed"
      println(s"$status | Money: $money | Coins: $coins | Expected: $expectedCount | Got: $actualCount")
    }
  }


}

