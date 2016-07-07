object oosets {

  abstract class IntSet {
    def contains(x: Int): Boolean
    def incl(x: Int): IntSet
    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    def contains(x: Int): Boolean = false
    def union(other: IntSet): IntSet = other
    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < this.elem) this.left contains x
      else if (x > this.elem) this.right contains x
      else true // x == elem
    def incl(x: Int): IntSet =
      if (x < this.elem) new NonEmpty(elem, this.left incl x, this.right)
      else if (x > this.elem) new NonEmpty(elem, this.left, this.right incl x)
      else this
    def union(other: IntSet): IntSet =
      ((left union right) union other) incl elem
    override def toString = "{" + this.left + elem + this.right + "}"
  }

  val s = Empty

  val t1 = new NonEmpty(3, Empty, Empty)

  val t2 = t1 incl 4
}