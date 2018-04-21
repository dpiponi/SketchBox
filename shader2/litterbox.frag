void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    float u = gl_PointCoord.x;
    float v = gl_PointCoord.y;
    vec2 f = 2.0*vec2(u-0.5, v-0.5);
    float ff = 1.0-dot(f, f);
    float z = sqrt(ff);
    float c0 = dot(vec3(1.0, 1.0, 2.0), vec3(f.x, f.y, z))/2.0;
    float c1 = dot(vec3(-1.0, 0.5, 1.0), vec3(f.x, f.y, z))/2.0;
    c0 = max(0.0, c0);
    c1 = max(0.0, c1);
    float c = c0+c1+0.2;
    fragColor = vec4(vec3(1.0,0.5,0.0)*c0+vec3(0.0,0.5,1.0)*c1,ff>0 ? 1 : 0);
}
