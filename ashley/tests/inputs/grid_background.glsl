public uniform vec2 u_resolution;
public uniform vec2 u_scroll_offset;

public uniform float u_zoom;

@location(0) in vec2 i_position;
@location(0) out vec4 out_color;

public void main() {
    vec2 position = i_position * vec2(1.0, -1.0) * u_resolution * 0.5;
    float vignette = clamp(0.7 * length(i_position), 0.0, 1.0);
    out_color = mix(
        vec4(0.0, 0.47, 0.9, 1.0),
        vec4(0.0, 0.1, 0.64, 1.0),
        vec4(vignette)
    );
    // TODO: properly adapt the grid while zooming in and out.
    float grid_scale = 5.0;
    if (u_zoom < 2.5) {
        grid_scale = 1.0;
    }
    vec2 pos = position + u_scroll_offset * u_zoom;

    if (mod(pos.x, 20.0 / grid_scale * u_zoom) <= 1.0 || mod(pos.y, 20.0 / grid_scale * u_zoom) <= 1.0) {
        out_color *= 1.2;
    }

    if (mod(pos.x, 100.0 / grid_scale * u_zoom) <= 2.0 || mod(pos.y, 100.0 / grid_scale * u_zoom) <= 2.0) {
        out_color *= 1.2;
    }
}
