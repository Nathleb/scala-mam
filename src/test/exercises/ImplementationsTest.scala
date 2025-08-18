import Implementations.*
import org.scalatest.funsuite.AnyFunSuite

class ImplementationsTest extends AnyFunSuite {

  test("pascal should return the correct value for a given row and column") {
    assert(pascal(0, 0) == 1)
    assert(pascal(0, 1) == 1)
    assert(pascal(1, 1) == 1)
    assert(pascal(1, 2) == 2)
    assert(pascal(2, 4) == 6)
    assert(pascal(4, 8) == 70)
  }

  test("balance should correctly check for balanced parentheses with foldLeft") {
    assert(balance(List('(', ')')))
    assert(balance(List('(', ')', '(', ')')))
    assert(balance(List(')', '(', ')')) === false)
    assert(balance(List('(', '(')) === false)
    assert(balance(List('(', '(', ')', ')')))
    assert(balance(List(')', '(')) === false)
    assert(balance(List('(', 'a', ')')))
    assert(balance(List('(', ')', '(', 'a', 'a', ')')))
    assert(balance(List.empty[Char]))
  }

  test("recursiveBalance should correctly check for balanced parentheses with recursion") {
    assert(recursiveBalance(List('(', ')')))
    assert(recursiveBalance(List('(', ')', '(', ')')))
    assert(recursiveBalance(List(')', '(', ')')) === false)
    assert(recursiveBalance(List('(', '(')) === false)
    assert(recursiveBalance(List('(', '(', ')', ')')))
    assert(recursiveBalance(List(')', '(')) === false)
    assert(recursiveBalance(List('(', 'a', ')')))
    assert(recursiveBalance(List('(', ')', '(', 'a', 'a', ')')))
    assert(recursiveBalance(List.empty[Char]))
  }


  test("countChange should handle base cases correctly") {
    assert(countChange(0, List(1, 2, 5)) == 1)
    assert(countChange(0, List.empty[Int]) == 1)
    assert(countChange(-1, List(1, 2)) == 0)
    assert(countChange(5, List.empty[Int]) == 0)
  }

  test("countChange should handle single coin denominations") {
    assert(countChange(4, List(1)) == 1)
    assert(countChange(4, List(2)) == 1)
    assert(countChange(3, List(2)) == 0)
    assert(countChange(5, List(5)) == 1)
  }

  test("countChange should correctly count unique combinations of coins") {
    assert(countChange(4, List(1, 2)) == 3)
    assert(countChange(4, List(1, 2, 3)) == 4)
    assert(countChange(6, List(1, 2, 5)) == 5)
    assert(countChange(17, List(1, 2, 5)) == 16)
  }

  test("countChange should handle impossible combinations") {
    assert(countChange(3, List(5, 10)) == 0)
    assert(countChange(1, List(2, 3, 5)) == 0)
    assert(countChange(7, List(3, 5)) == 0)
  }

  test("countChange should be independent of coin order") {
    assert(countChange(6, List(1, 2, 5)) == countChange(6, List(5, 1, 2)))
    assert(countChange(6, List(1, 2, 5)) == countChange(6, List(2, 5, 1)))
    assert(countChange(10, List(1, 5, 10)) == countChange(10, List(10, 1, 5)))
  }

  test("countChange should handle medium-sized problems") {
    assert(countChange(25, List(1, 5, 10, 25)) == 13)
    assert(countChange(30, List(1, 5, 10, 25)) == 18)
    assert(countChange(50, List(1, 5, 10, 25)) == 49)
  }

  test("countChange should handle duplicate coins the same as unique coins") {
    assert(countChange(4, List(1, 1, 2)) == countChange(4, List(1, 2)))
    assert(countChange(6, List(2, 2, 3)) == countChange(6, List(2, 3)))
  }
}