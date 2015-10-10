object TreeBuilder {
  val lf1 = Leaf(1)
  val lf2 = Leaf(2)
  val lf3 = Leaf(3)
  val lf4 = Leaf(4)
  val lf5 = Leaf(5)
  val lf6 = Leaf(6)

  val br3 = Branch(lf3, lf4)
  val br4 = Branch(lf5, lf6)
  val br1 = Branch(lf1, lf2)
  val br2 = Branch(br3, br4)

  val rt = Branch(br1, br2)

  def build = rt
}