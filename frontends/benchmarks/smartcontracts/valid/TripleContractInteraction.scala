import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait UnknownInterfaceA extends Contract {
  def balance(a: Address): Uint256
  def transfer(from: Address, to: Address, amount: Uint256)

  final def invariant() = true
}

trait OCMB extends Contract {
  var balance: Uint256
  var target: Address

  final def invariant() = 
    balance >= Uint256.ONE &&
    Environment.contractAt(target).isInstanceOf[UnknownInterfaceA]

  final def transfer(to: Address, amount: Uint256):Unit = {
    require(
      // Needed to avoid overflow. Temporary
      balance <= Uint256("30")
    )

    if(balance > amount + Uint256.ONE) {
      balance = balance - amount
      Environment.contractAt(target).asInstanceOf[UnknownInterfaceA].transfer(this.addr, to, amount)
    }
  }

  final def balance(a: Address) = {
    Environment.contractAt(target).asInstanceOf[UnknownInterfaceA].balance(a)
  }
}

trait OCMA extends Contract {
  val target:Address

  final def invariant() = Environment.contractAt(target).isInstanceOf[OCMB] &&
                          Environment.contractAt(target).asInstanceOf[OCMB].invariant()

  final def foo() = {
    Environment.contractAt(target).asInstanceOf[OCMB].transfer(this.addr, Uint256.ONE)
  }

}
