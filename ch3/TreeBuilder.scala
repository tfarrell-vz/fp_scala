object TreeBuilder {
  val lf1 = Leaf(1)
  val lf2 = Leaf(2)
  val lf3 = Leaf(3)
  val lf4 = Leaf(4)

  val br1 = Branch(lf1, lf2)
  val br2 = Branch(lf3, lf4)

  val rt = Branch(br1, br2)

  def build = rt
}