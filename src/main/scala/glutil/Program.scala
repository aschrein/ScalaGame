package glutil
import com.jogamp.opengl.GL2ES2._
import com.jogamp.opengl.{GL2, GL4}
import linalg.{Mat, vec2, vec3, vec4}
/**
  * Created by anton on 12/9/2016.
  */
object Program {
	def enumerateLines ( text : String ) = text.split ( "\n" ).zipWithIndex.map ( pair => pair._2 + ":" + pair._1 + "\n" ).reduce ( _ + _ )
	def validateShader ( gl : GL4, shader : Int ) : String = {
		val ival = Array ( 0 )
		gl.glGetShaderiv ( shader, GL_COMPILE_STATUS, ival, 0 )
		if ( ival ( 0 ) == 0 ) {
			gl.glGetShaderiv ( shader, GL_INFO_LOG_LENGTH, ival, 0 )
			val bbuf = new Array[ Byte ]( ival ( 0 ) )
			gl.glGetShaderInfoLog ( shader, ival ( 0 ), ival, 0, bbuf, 0 )
			new String ( bbuf )
		} else null
	}
	def validateProgram ( gl : GL4, program : Int ) : String = {
		val ival = Array ( 0 )
		gl.glGetProgramiv( program, GL_LINK_STATUS, ival, 0 )
		if ( ival ( 0 ) == 0 ) {
			gl.glGetProgramiv ( program, GL_INFO_LOG_LENGTH, ival, 0 )
			val bbuf = new Array[ Byte ]( ival ( 0 ) )
			gl.glGetProgramInfoLog( program, ival ( 0 ), ival, 0, bbuf, 0 )
			new String ( bbuf )
		} else null
	}
}
class Program ( gl : GL4, frag_text : String, vert_text : String ) extends GLObject {
	val program = gl.glCreateProgram ( )
	val vs = gl.glCreateShader ( GL_VERTEX_SHADER )
	gl.glShaderSource ( vs, 1, Array ( vert_text ), null )
	gl.glCompileShader ( vs )
	val vs_validation = Program.validateShader( gl , vs )
	if( vs_validation != null ) {
		println( Program.enumerateLines( vert_text ) )
		println( vs_validation )
		throw new Exception( "invalid vertex shader source" )
	}
	val fs = gl.glCreateShader ( GL_FRAGMENT_SHADER )
	gl.glShaderSource ( fs, 1, Array ( frag_text ), null )
	gl.glCompileShader ( fs )
	val fs_validation = Program.validateShader( gl , fs )
	if( fs_validation != null ) {
		println( Program.enumerateLines( frag_text ) )
		println( fs_validation )
		throw new Exception( "invalid fragment shader source" )
	}
	gl.glAttachShader ( program, vs )
	gl.glAttachShader ( program, fs )
	gl.glLinkProgram ( program )
	gl.glValidateProgram( program )
	val prog_validation = Program.validateProgram( gl , program )
	if( prog_validation != null ) {
		println( Program.enumerateLines( frag_text ) )
		println("__________________")
		println( Program.enumerateLines( vert_text ) )
		println( prog_validation )
		throw new Exception( "program linkage error" )
	}
	def attribute ( name : String ) = {
		gl.glGetAttribLocation ( program, name )
	}
	def uniform ( name : String ) = {
		gl.glGetUniformLocation ( program, name )
	}
	def update ( name : String, v : Any ) = v match {
		case vec3 ( x, y, z ) => gl.glUniform3f ( uniform ( name ), x, y, z )
		case vec2 ( x, y ) => gl.glUniform2f ( uniform ( name ), x, y )
		case vec4 ( x, y, z, w ) => gl.glUniform4f ( uniform ( name ), x, y, z, w )
		case x : Float => gl.glUniform1f ( uniform ( name ), x )
		case x : (Float, Float) => gl.glUniform2f ( uniform ( name ), x._1, x._2 )
		case x : (Float, Float, Float) => gl.glUniform3f ( uniform ( name ), x._1, x._2, x._3 )
		case x : (Float, Float, Float, Float) => gl.glUniform4f ( uniform ( name ), x._1, x._2, x._3, x._4 )
		case x : Int => gl.glUniform1i ( uniform ( name ), x )
		case x : (Int, Int) => gl.glUniform2i ( uniform ( name ), x._1, x._2 )
		case x : (Int, Int, Int) => gl.glUniform3i ( uniform ( name ), x._1, x._2, x._3 )
		case x : (Int, Int, Int, Int) => gl.glUniform4i ( uniform ( name ), x._1, x._2, x._3, x._4 )
		case Mat ( n, m, arr ) if n == m => n match {
			case 2 => gl.glUniformMatrix2fv ( uniform ( name ), 1, true, arr, 0 )
			case 3 => gl.glUniformMatrix3fv ( uniform ( name ), 1, true, arr, 0 )
			case 4 => gl.glUniformMatrix4fv ( uniform ( name ), 1, true, arr, 0 )
			case _ => throw new Exception ( "unsupported matrix size" )
		}
		case _ => throw new Exception ( "not implemented shader uniform" )
	}
	def bind ( ) = {
		gl.glUseProgram ( program )
	}
	override def dispose ( ) : Unit = {
		gl.glDeleteShader ( vs )
		gl.glDeleteShader ( fs )
		gl.glDeleteProgram ( program )
	}
}
