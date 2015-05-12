// Recursive function to get the nth Fibonacci number
// No loops allowed: Think functionally.

object MainModule {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(prev: Int, next: Int, count: Int): Int = {
      if (count == 0) prev
      else loop(next, prev+next, count-1)
    }
    loop(0, 1, n)
  }
}


// vim: set ts=2 sw=2 et sts=2:
