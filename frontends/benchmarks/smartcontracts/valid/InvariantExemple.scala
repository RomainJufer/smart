import stainless.smartcontracts._
import stainless.annotation._

trait ContractA extends Contract {

}

trait ContractB extends Contract {
    val target:Address

    final def invariant() = Environment.contractAt(target).isInstanceOf[ContractA]
}


