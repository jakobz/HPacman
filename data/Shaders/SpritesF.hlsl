#version 120

varying vec2 uvOut;

uniform sampler2D tex;

void main(void)
{
    gl_FragColor = texture2D(tex, uvOut);
}