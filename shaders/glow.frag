#version 120

void main() {
    float u = gl_PointCoord.x;
    float v = gl_PointCoord.y;
    vec2 f = 2.0*vec2(u-0.5, v-0.5);
    float ff = exp(-2.0*dot(f, f));
    gl_FragColor = vec4(ff, ff, ff, ff);
}
