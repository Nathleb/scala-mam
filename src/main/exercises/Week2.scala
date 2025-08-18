import scala.annotation.tailrec

type Set = Int => Boolean

object Week2 {

  def singletonSet(value: Int): Set = {
    (a: Int) => a == value
  }

  def union(s: Set, t: Set): Set = {
    (a: Int) => s(a) || t(a)
  }

  def intersect(s: Set, t: Set): Set = {
    (a: Int) => s(a) && t(a)
  }

  def dif(s: Set, t: Set): Set = {
    (a: Int) => s(a) && !t(a)
  }

  def filter(s: Set, p: Int => Boolean): Set = {
    (a: Int) => s(a) && p(a)
  }

  def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, x => f(x) == y)

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def forall(s: Set, p: Int => Boolean): Boolean = {
    @tailrec
    def inForall(currentTestedInt: Int): Boolean = {
      if (currentTestedInt > 1000) {
        true
      } else if (contains(s, currentTestedInt) && !p(currentTestedInt)) {
        false
      } else {
        inForall(currentTestedInt + 1)
      }
    }

    inForall(-1000)
  }

  def contains(s: Set, elem: Int): Boolean = s(elem)


}
