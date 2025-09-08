package recfun

import org.scalatest.funsuite.AnyFunSuite


class CountChangeSuite extends AnyFunSuite {
  import Main.countChangeCount
  test("countChange: example given in instructions") {
    assert(countChangeCount(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChangeCount(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChangeCount(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChangeCount(300,List(500,5,50,100,20,200,10)) === 1022)
  }
}
