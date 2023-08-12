// http://www.pouet.net/prod.php?which=57245
// If you intend to reuse this shader, please add credits to 'Danilo Guanabara'

uniform float time;
uniform uint width;
uniform uint height;

@location(0) in vec2 uv;
@location(0) out vec4 color;

vec4 mainImage(vec2 fragCoord) {
	vec3 c;
	vec2 r = vec2(width, height);
	float l;
	float z=time;
	for(int i = 0; i < 3; i++) {
		vec2 uv;
		vec2 p = fragCoord.xy/r;
		uv = p;
		p -= .5;
		p.x *= r.x/r.y;
		z += .07;
		l = length(p);
		uv += p/l*(sin(z)+1.)*abs(sin(l*9.-z-z));
		c[i]=.01/length(mod(uv,1.)-.5);
	}
	return vec4(c/l,1.0);
}

@fragment void fs_main() {
    color = mainImage(uv * vec2(width, height));
}
