package glutil

/**
  * Created by anton on 12/9/2016.
  */
trait GLObject {
	def dispose() : Unit
	override def finalize() = dispose()
}
