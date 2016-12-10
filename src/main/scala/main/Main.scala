package main
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.{Graphics, Graphics2D}
import java.nio.{ByteBuffer, ByteOrder}
import javax.swing.JFrame
import com.jogamp.opengl.GL._
import com.jogamp.opengl.GL2ES2._
import com.jogamp.opengl._
import com.jogamp.opengl.awt.GLJPanel
import glutil.Program
import linalg.vec3
import utilities.Camera
object Cube {
	val d = GL_VERTEX_SHADER
	var vao = 0
	var vbo = 0
	var ibo = 0
	def init ( gl : GL4 ) = {
		val arr : Array[ Int ] = new Array[ Int ]( 3 )
		gl.glGenVertexArrays ( 1, arr, 0 )
		vao = arr ( 0 )
		gl.glGenBuffers ( 2, arr, 0 )
		vbo = arr ( 0 )
		ibo = arr ( 1 )
		gl.glBindVertexArray ( vao )
		gl.glBindBuffer ( GL.GL_ARRAY_BUFFER, vbo )
		gl.glEnableVertexAttribArray ( 0 )
		gl.glVertexAttribPointer ( 0, 3, GL_FLOAT, false, 12, 0 )

		val vertex_buffer = ByteBuffer.allocate ( 8 * 3 * 4 ).order ( ByteOrder.LITTLE_ENDIAN )
		for ( i <- 0 until 8 ) {
			vertex_buffer.putFloat ( ( i & 1 ) * 2 - 1 )
			vertex_buffer.putFloat ( ( i >> 1 & 1 ) * 2 - 1 )
			vertex_buffer.putFloat ( ( i >> 2 & 1 ) * 2 - 1 )
		}
		vertex_buffer.rewind ( )
		gl.glBufferData ( GL.GL_ARRAY_BUFFER, vertex_buffer.limit, vertex_buffer, GL_STATIC_DRAW )
		gl.glBindBuffer ( GL.GL_ELEMENT_ARRAY_BUFFER, ibo )
		val index_array = Array [ Int ](
			0, 2, 3, 3, 1, 0, 0, 5, 4, 0, 1, 5,
			0, 6, 2, 0, 4, 6, 2, 6, 7, 2, 7, 3,
			7, 5, 3, 5, 1, 3, 4, 7, 6, 4, 5, 7
		)
		val index_buffer = ByteBuffer.allocate ( index_array.length * 4 ).order ( ByteOrder.LITTLE_ENDIAN )
		for ( i <- index_array ) index_buffer.putInt ( i )
		index_buffer.rewind ( )
		gl.glBufferData ( GL.GL_ELEMENT_ARRAY_BUFFER, index_buffer.limit ( ), index_buffer, GL_STATIC_DRAW )
		gl.glBindVertexArray ( 0 )
	}
	def draw ( gl : GL4 ) : Unit = {
		gl.glBindVertexArray ( vao )
		gl.glDrawElements ( GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0 )
		gl.glBindVertexArray ( 0 )
	}
}
object Main {
	def checkGLError ( gl : GL ) = {
		val err = gl.glGetError ( )
		if ( err != 0 ) {
			val g = glu.GLU.createGLU ( gl )
			println ( g.gluErrorString ( err ) )
		}
	}
	def main ( args : Array[ String ] ) : Unit = {
		val frame = new JFrame ( )
		val glprofile = GLProfile.get ( GLProfile.GL2 )
		val glcapabilities = new GLCapabilities ( glprofile )
		glcapabilities.setSampleBuffers ( true )
		glcapabilities.setHardwareAccelerated ( true )
		glcapabilities.setDoubleBuffered ( true )
		glcapabilities.setNumSamples ( 16 )
		val glpanel = new GLJPanel ( glcapabilities ) {
			override def paint ( g : Graphics ) : Unit = {
				val g2d = g match {
					case g : Graphics2D => g
					case _ => throw new ClassCastException
				}
				super.paint ( g )
				//g2d.drawLine( 0, 0, 100, 100 )
			}
		}
		var phi = 0.0f
		var theta = 0.0f
		var last_x = 0
		var last_y = 0
		glpanel.setDoubleBuffered ( true )
		glpanel.addMouseMotionListener ( new MouseMotionListener {
			override def mouseMoved ( mouseEvent : MouseEvent ) : Unit = {}
			override def mouseDragged ( mouseEvent : MouseEvent ) : Unit = {
				val k = 0.01f
				phi += ( mouseEvent.getX - last_x ) * k
				theta += ( mouseEvent.getY - last_y ) * k
				last_x = mouseEvent.getX
				last_y = mouseEvent.getY
				theta = Math.max(  -Math.PI / 2 + 0.01f  , Math.min( Math.PI / 2 - 0.01f , theta ) ).toFloat
			}
		} )
		glpanel.addMouseListener ( new MouseListener {
			override def mouseExited ( mouseEvent : MouseEvent ) : Unit = {}
			override def mouseClicked ( mouseEvent : MouseEvent ) : Unit = {}
			override def mouseEntered ( mouseEvent : MouseEvent ) : Unit = {}
			override def mousePressed ( mouseEvent : MouseEvent ) : Unit = {
				last_x = mouseEvent.getX
				last_y = mouseEvent.getY
			}
			override def mouseReleased ( mouseEvent : MouseEvent ) : Unit = {}
		} )
		glpanel.addGLEventListener ( new GLEventListener ( ) {
			var program : Program = null
			override def reshape ( glautodrawable : GLAutoDrawable, x : Int, y : Int, width : Int, height : Int ) : Unit = {
				val gl = glautodrawable.getGL.getGL2
				gl.glBindFramebuffer ( GL_FRAMEBUFFER, 0 )
				gl.glViewport ( 0, 0, width, height )
				gl.glClearColor ( 0.0f, 0.0f, 0.0f, 1.0f )
				gl.glClearDepth ( 1.0 )
				gl.glClear ( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT )
				//gl.glFrontFace ( GL_FRONT_AND_BACK )
				gl.glDisable ( GL_CULL_FACE )
				gl.glEnable ( GL_DEPTH_TEST )
			}
			override def init ( glautodrawable : GLAutoDrawable ) : Unit = {
				val gl = glautodrawable.getGL.getGL4
				Cube.init ( gl )
				//checkGLError( gl )
				try {
					program = new Program ( gl,
						"""
						  |#version 330
						  |in vec3 f_pos;
						  |void main()
						  |{
						  |		gl_FragColor = vec4( abs( f_pos ) , 1.0 );
						  |}
						""".
							stripMargin,
						"""
						  |#version 330
						  |layout( location = 0 ) in vec3 pos;
						  |out vec3 f_pos;
						  |uniform mat4 viewproj;
						  |void main()
						  |{
						  |		/*vec3 pos[ 3 ] = vec3[](
						  |  	vec3( -1.0f , -1.0f , 0.0f ) ,
						  |   	vec3( 1.0f , -1.0f , 0.0f ) ,
						  |    	vec3( 0.0f , 1.0f , 0.0f )
						  |  );[ gl_VertexID ]*/
						  |  	f_pos = pos;
						  |		gl_Position = viewproj * vec4( pos , 1.0 );
						  |}
						""".
							stripMargin
					)
				} catch {
					case e : Exception => System.exit ( -1 )
				}
				//checkGLError( gl )
			}
			override def dispose ( glautodrawable : GLAutoDrawable ) : Unit = {
			}
			override def display ( glautodrawable : GLAutoDrawable ) : Unit = {
				val gl = glautodrawable.getGL.getGL4
				gl.glClearColor ( 0.0f, 0.0f, 0.0f, 0.0f )
				gl.glClearDepth ( 1.0 )
				gl.glClear ( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )
				program.bind ( )
				val viewproj = Camera.perspLook ( vec3 ( Math.cos ( theta ) * Math.cos ( phi ) toFloat, Math.cos ( theta ) * Math.sin ( phi ) toFloat, Math.sin ( theta ).toFloat ) * 3, vec3 ( 0.0f, 0.0f, 0.0f ), vec3 ( 0.0f, 0.0f, 1.0f ), 1.0f, 1.0f, 0.1f, 100.0f )
				program ( "viewproj" ) = viewproj
				Cube.draw ( gl )
			}
		} )
		frame.add ( glpanel )
		frame.setVisible ( true )
		frame.setSize ( 512, 512 )
		frame.setDefaultCloseOperation ( javax.swing.WindowConstants.EXIT_ON_CLOSE )
		frame.setVisible ( true )
		frame.setFocusable ( true )

		while ( frame.isActive ) {
			frame.repaint ( )

			Thread.sleep ( 1 )
		}
	}
}