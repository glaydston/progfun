object intsets {
  new InSet
}

abstract class InSet {
  def incl(x: Int): InSet
  def contains(x: Int): Boolean
}

class Empty extends InSet {

}

