#version 120

attribute vec4 vPosition;
uniform float pointSize;

void main() {
    gl_Position = vPosition;
    gl_PointSize = pointSize;
}
