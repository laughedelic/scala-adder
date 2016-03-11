package laughedelic.so.test

import laughedelic.so._, adder._

case object testContext {

  val add0Numbers = adder_[_0](0) // always 0
  val add1Number  = adder_[_1](0) // identity

  val add3Numbers = adder_[_3](0)
  val add4Numbers = adder_[_4](0)
}

class adderTest extends org.scalatest.FunSuite {
  import testContext._

  test("Edge cases") {

    assert{ add0Numbers === 0 }

    assert{ add1Number(-10323) === -10323 }
    assert{ add1Number(42) === 42 }
  }

  test("Adding some numbers") {

    assert{ add3Numbers(1)(2)(3) === 6 }

    assert{ add4Numbers(2)(-2)(3)(-5) === -2 }
  }
}
