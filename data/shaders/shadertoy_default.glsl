uniform float time;
uniform uint width;
uniform uint height;

in vec2 uv;
out vec4 color;

void main() {
    color = vec4(time + 0.5 + 0.5 * cos(time + uv.xyx + vec3(0.0,2.0,4.0)), 1.0);
}
