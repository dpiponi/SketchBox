#version 120

varying float ffade;

void main() {
    gl_FragColor = vec4(ffade, ffade, ffade, 1.0);
}
