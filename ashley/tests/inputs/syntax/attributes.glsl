

@location(0)        // @attribute(<literal>)
@gui(slider(float,min=0.0,max=1.0))
@rename("test")
@rename(from="test",to="test2")
in vec2[10] stride(16) test;


uniform mat3 row_major [10] stride(64) test;

@location(0) in vec2 uv;
@location(0) out vec4 color;