#version 120

attribute vec4 vPosition;
uniform float pointSize;
uniform mat4 transform;

void main() {
   gl_Position = transform*vPosition;
   gl_PointSize = pointSize;
}
