import stainless.smartcontracts._
import stainless.annotation._
import stainless.lang.StaticChecks._

object PositiveUint {
  	case class PositiveUint() extends Contract {
			@solidityPure
  		def test(@ghost a: Uint256) = {
    		assert(a >= Uint256.ZERO)
  		}
	}
}
