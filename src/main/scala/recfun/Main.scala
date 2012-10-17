package recfun
import common._

object Main {
  
  //def shiftRight(row: List[Int]): List[Int] = 0::row
//def shiftLeft(row:List[Int]):List[Int]=row:::List(0)

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println("  ")
    }
    //println(countChange(money, coins.tail))
    
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int):Int = {
   
    def calculate(c: Int, r: Int): Int= 
      if(c==0||c==r)   1  
     
      else calculate(c-1,r-1) + calculate(c,r-1)
       
    calculate(c,r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
     def balance(chars: List[Char], str: String): Boolean = {
      if (chars.isEmpty)
        str.isEmpty
      else if (chars.head == ')')
        balance(chars.tail, chars.head + str)
      else if (chars.head == '(')
        !str.isEmpty && balance(chars.tail, str.tail)
      else
        balance(chars.tail, str)
    }

    balance(chars, "")
    
  }
  
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else
      if (money < 0 || coins.isEmpty)
        0
      else
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
     // println(countChange(0,coins.tail))
      
        
  }
  
}
