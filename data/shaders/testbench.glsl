



uniform float time;
uniform uint width;
uniform uint height;

struct TestStruct {
    int a;
    float b;
    float ba;
}

@location(0) in vec2 uv;
@location(0) out vec4 color;

@fragment void fs_main() {
    color = vec4(0.5 + 0.5 * cos(time + uv.xyx + vec3(0.0,2.0,4.0)), 1.0);
    return 0;
}



