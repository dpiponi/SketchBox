#version 120

attribute vec4 vPosition;
attribute vec4 color;
uniform float pointSize;
uniform mat4 transform;

void main() {
   gl_Position = transform*vPosition;
   gl_PointSize = pointSize;
   gl_FrontColor = color;
}
