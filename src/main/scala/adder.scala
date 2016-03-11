package laughedelic.so

object adder {

  // any nested type would work:
  type Nat = Option[_]

  type Zero = None.type
  type Succ[N <: Nat] = Some[N]

  // enough for examples:
  type _0 = Zero
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  // etc...



  trait AdderType[N <: Nat] {

    type Out
    def apply(i: Int): Out
  }

  case object AdderType {

    implicit def zero:
        AdderType[_0] { type Out = Int } =
    new AdderType[_0] {

      type Out = Int
      def apply(i: Int): Out = i
    }

    implicit def succ[N <: Nat, NOut](
      implicit prev: AdderType[N] { type Out = NOut }
    ):  AdderType[Succ[N]] { type Out = Int => NOut } =
    new AdderType[Succ[N]] {

      type Out = Int => NOut
      def apply(i: Int): Out = k => prev(i + k)
    }
  }

  def adder_[N <: Nat](initial: Int)(
    implicit adderFunction: AdderType[N]
  ): adderFunction.Out = adderFunction(initial)



  ////////////////////////////////////////
  // Or, alternatively, without using .Out

  // constructing "values" to derive its type arg
  case class NatVal[N <: Nat]()
  // just a convenience function
  def nat[N <: Nat]: NatVal[N] = NatVal[N]()


  // we need one more type arg NOut, but then we need a way to derive N (using NatVal)
  def adder[N <: Nat, NOut](n: NatVal[N])(
    implicit adderFunction: AdderType[N] { type Out = NOut }
  ): Int => NOut = init => adderFunction(init)
}
