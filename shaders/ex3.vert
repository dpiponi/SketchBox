#version 120

attribute vec4 vPosition;
uniform float pointSize;
attribute float fade;
varying float ffade;

void main() {
    gl_Position = vPosition;
    gl_PointSize = pointSize;
    ffade = fade;
}
