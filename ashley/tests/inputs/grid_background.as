uniform u_resolution: vec2;
uniform u_scroll_offset: vec2;
uniform u_zoom: vec2;

in i_position: vec2;
out o_color: vec4;

fn main() {
    let position : f32 = v_position * vec2(1.0, -1.0) * u_resolution * 0.5;
    let vignette : f32 = clamp(0.7 * length(v_position), 0.0, 1.0);
    out_color = mix(
        vec4(0.0, 0.47, 0.9, 1.0),
        vec4(0.0, 0.1, 0.64, 1.0),
        vignette
    );
    // TODO: properly adapt the grid while zooming in and out.
    let grid_scale = 5.0;
    if u_zoom < 2.5 {
        grid_scale = 1.0;
    }
    let pos = px_position + u_scroll_offset * u_zoom;

    if mod(pos.x, 20.0 / grid_scale * u_zoom) <= 1.0 || mod(pos.y, 20.0 / grid_scale * u_zoom) <= 1.0 {
        out_color *= 1.2;
    }

    if mod(pos.x, 100.0 / grid_scale * u_zoom) <= 2.0 || mod(pos.y, 100.0 / grid_scale * u_zoom) <= 2.0 {
        out_color *= 1.2;
    }
}
