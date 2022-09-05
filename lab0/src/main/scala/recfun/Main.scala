package recfun

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println()
    println(balance("<li>:-)</li>".toList))
    println()
    println(countChange(5, List(1, 2, 5)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c > r || r < 0 || c < 0) {
      throw new RuntimeException("Input incorrect")
    }

    if(c == 0 || c == r)
      return 1
    else
      return pascal(c, r-1) + pascal(c - 1, r-1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def bl(hd: Char, tl: List[Char] , op: Int, cl: Int): (Int, Int) = {
      if(tl.isEmpty) {
        if(hd == '(')
          return (op+1, cl)
        if(hd == ')')
          return (op, cl + 1)
        return (op, cl)
      }

      if(hd == '(') {
        return bl(tl.head, tl.tail, op+1, cl)
      } else {
        if(hd == ')') {
          if(op > cl) {
            return bl(tl.head, tl.tail, op, cl+1)
          }
          else
            return (-1, -1)
        } else
            return bl(tl.head, tl.tail, op, cl)
      }
    }

    val (op, cl) = bl(chars.head, chars.tail, 0, 0)

    if(op == -1)
      return false
    if(op == cl)
      return true
    else
      return false

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    if(money == 0)
      return 1
    if(coins.length == 0)
      return 0

    if (money - coins.max >= 0)
      return countChange(money, coins.reverse.tail.reverse) + countChange(money - coins.max, coins)
    else
      return countChange(money, coins.reverse.tail.reverse)
  }

}
