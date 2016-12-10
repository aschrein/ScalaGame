/**
  * Created by anton on 12/8/2016.
  */
import linalg.Mat
import org.junit._
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit
class linalg  extends AssertionsForJUnit {
	@Test def test() = {
		val arr = new Array[ Float ]( 100 )
		for( i <- arr.indices ) {
			arr ( i ) = Math.random().toFloat * 10
		}
		//arr ( 1 ) = 0.0f
		val mat = new Mat( 10 , 10 , arr )
		val inv = mat.inv
		mat.print()
		inv.print()
		( mat * inv ).print()
		assertTrue(( mat * inv ).identity )
	}
}
