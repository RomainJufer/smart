import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang.StaticChecks._
import stainless.lang.ghost

trait A extends Contract {
  var target: Address

  def f(): Unit = {
    Environment.contractAt(target).asInstanceOf[B].g()
    assert(Environment.contractAt(target).asInstanceOf[B].x >= 0)
  }

  def invariant() = {
    Environment.contractAt(target).isInstanceOf[B] &&
    Environment.contractAt(target).asInstanceOf[B].invariant()
  }
}

trait B extends Contract {
  var x: BigInt
  def g(): Unit
  def invariant(): Boolean = x >= 0
}