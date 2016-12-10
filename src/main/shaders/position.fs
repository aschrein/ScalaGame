#version 330
in vec3 f_pos;
void main()
{
		gl_FragColor = vec4( abs( f_pos ) , 1.0 );
}