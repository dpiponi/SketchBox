#version 120

attribute vec4 vPosition;

void main() {
   gl_Position = vPosition;
   gl_PointSize = 8.0;
}
