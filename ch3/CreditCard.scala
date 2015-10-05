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

