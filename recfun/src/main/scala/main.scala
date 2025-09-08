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
  def testBalance(): Unit = {
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
}

