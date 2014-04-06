uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

float blob(vec2 c, float r1, float r2){
	return 	1.-smoothstep(r1,r2,distance(c,gl_FragCoord.xy));
}

void main( void ) {

	float b = 0.0;
	for (int n = 0; n < 14; n++)
	{
		b += blob(vec2(sin((150.+time)*float(n)*0.009), cos((150.+time)*float(14-n)*0.025))*resolution*0.4+resolution/2.0,10.,200.);
	}
	b-= 1.0*blob(mouse*resolution,10.,150.);
	
	vec3 color = step(0.5,b)*vec3((cos(0.3*time)+1.)*0.25*(step(0.4,1.-cos(b*0.5))+step(0.6,sin(b*15.))),
					    (sin(0.3*time)+1.)*0.25*(step(0.1,sin(b*9.))+step(0.3,cos(b*9.))),
					    (sin(0.3*time+2.)+1.)*0.5*(step(0.1,sin(b*5.))));
	color += vec3(step(0.5,b)-step(0.52,b));
	
	gl_FragColor = vec4(color,1.0);
}


