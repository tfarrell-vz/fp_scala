/**
 * www.seas.upenn.edu/~cis194/hw/01-intro.pdf
 */

/**
 * Exercise 1
 */

def lastDigit(num: Int): Int = num % 10
def dropLastDigit(num: Int): Int = num / 10

/**
  * Exercise 2
  */

def toDigits(num: Int): List[Int] = {
  def loop(num: Int, acc: List[Int]):List[Int] = {
    if (num == 0) acc
    else if (num < 0) List[Int]()
    else loop(dropLastDigit(num), lastDigit(num)::acc)
  }
  loop(num, List[Int]())
}

def toRevDigits(nums: List[Int]):List[Int] = nums.reverse

def doubleEveryOther(nums:List[Int]):List[Int] = {
  val len = nums.length

  def loop(index: Int, con: List[Int], acc: List[Int]):List[Int] = {
    if (index > len) acc.reverse
    else {
      if (index % 2 == 0) {
        loop(index+1, con.tail, (2 * con.head)::acc)
      }
      else {
        loop(index+1, con.tail, con.head::acc)
      }
    }
  }
  loop(1, nums, List[Int]())
}