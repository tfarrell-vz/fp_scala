object Sorter {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n+1 >= as.length) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }
}