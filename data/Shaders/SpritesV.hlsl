#version 140
 
in vec2 position;
in vec2 uv;

out vec2 uvOut;

uniform Transformation {
    mat4 projection_matrix;
    mat4 modelview_matrix;
};

void main() {
    uvOut = uv;
    gl_Position = vec4(position.x / 400 - 1, - position.y / 300 + 1, 0.0, 1.0);
}