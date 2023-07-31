uniform float time;
uniform float dummy;

in vec2 uv;
out vec4 color;

void main() {
    color = vec4(dummy + 0.5 + 0.5 * cos(time + uv.xyx + vec3(0.0,2.0,4.0)), 1.0);
}
