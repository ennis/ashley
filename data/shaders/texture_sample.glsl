uniform float time;
uniform uint width;
uniform uint height;

uniform texture2D tex;
// TODO const samplers?
uniform sampler nearest;

@location(0) in vec2 uv;
@location(0) out vec4 color;

@fragment void fs_main() {
    vec4 tex_color = textureSample(tex, nearest, uv);
    color = tex_color;
}
