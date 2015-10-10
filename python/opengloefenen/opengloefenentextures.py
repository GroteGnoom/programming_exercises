# --------------------------------------------------------------------------------
# Copyright (c) 2013 Mack Stone. All rights reserved.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# --------------------------------------------------------------------------------

"""
Modern OpenGL with python.
render a color triangle with pyopengl using freeglut.

@author: Mack Stone
"""

import ctypes
import PIL.Image
import numpy
from OpenGL.GL import *
from OpenGL.GL import shaders
from OpenGL.GLUT import *

VERTEX_SHADER = """
#version 330

layout (location=0) in vec4 position;
layout (location=1) in vec4 color;
layout (location=2) in vec2 texcoord;

smooth out vec4 theColor;
out vec2 Texcoord;

void main()
{
    gl_Position = vec4(position.x, position.y, 0,1);
    theColor = color;
    Texcoord = texcoord;
}
"""

FRAGMENT_SHADER = """
#version 330

smooth in vec4 theColor;
in vec2 Texcoord;
out vec4 outputColor;

uniform sampler2D tex;

void main()
{
    outputColor = texture(tex,Texcoord)* theColor;
}
"""
shaderProgram = None
VAO = None
tex=None

def initialize():
    global VERTEX_SHADER
    global FRAGMENT_SHADER
    global shaderProgram
    global VAO
    global tex
    # compile shaders and program
    vertexShader = shaders.compileShader(VERTEX_SHADER, GL_VERTEX_SHADER)
    fragmentShader = shaders.compileShader(FRAGMENT_SHADER, GL_FRAGMENT_SHADER)
    shaderProgram = shaders.compileProgram(vertexShader, fragmentShader)

    # triangle position and color

    vertexData = numpy.array([-0.5, 0.5, 0.0, 1, #linksboven: x,y, geenideez?, ydelendoorditw?
                            0.5, 0.5, 0.0, 1.0,#rechtsboven: x,
                            0.5, -0.5, 0.0, 1.0,#linksonder
                            -0.5, -0.5, 0.0, 1.0, #rechtsonder
                            0.1, 0.0, 0.0, 1.0,
                            0.0, 1.0, 0.0, 1.0,#kleur2?
                            0.0, 0.0, 1.0, 1.0,
                            1.0, 0.0, 1.0, 1.0,
                            -0.5,0.5, #2d texture coordinaten
                            -0.5,0.5,
                            0.5,-0.5
                            -0.5,-0.5],
                            dtype=numpy.float32)

    
    
    # create VAO
    VAO = glGenVertexArrays(1)
    glBindVertexArray(VAO)
    
    #elements
    elements=numpy.array([0,1,2,
                          2,3,0,
                          4,5,6,
                          6,7,4,
                          8,9,10,
                          10,11,8],dtype=numpy.uint32)
    ebo=glGenBuffers(1)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo)
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, elements.nbytes, elements, GL_STATIC_DRAW)
    
    #texturethings
    tex=glGenTextures(1)
    glBindTexture(GL_TEXTURE_2D,tex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    tempcolor=numpy.array([0.5,0.5,0.5,1],dtype=numpy.float32)
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR,tempcolor)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glGenerateMipmap(GL_TEXTURE_2D)
    #temppixels=numpy.array([1,1,1,  0,0,0,
    #                        1,1,1,  0,0,0],dtype=numpy.float32)
    #glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 2, 2, 0, GL_RGB, GL_FLOAT, temppixels)
    ##opening file
    
    im=PIL.Image.open("C:/Users/daniel/Documents/python/opengloefenen/pandas.bmp")
    try:
        ix, iy, image = im.size[0], im.size[1], im.tostring("raw", "RGB", 0, -1) 
    except SystemError: 
        ix, iy, image = im.size[0], im.size[1], im.tostring("raw", "RGBX", 0, -1)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, ix, iy, 0, GL_RGB,
              GL_UNSIGNED_BYTE, image)
    
    # create VBO
    VBO = glGenBuffers(1)
    glBindBuffer(GL_ARRAY_BUFFER, VBO)
    glBufferData(GL_ARRAY_BUFFER, vertexData.nbytes, vertexData, GL_STATIC_DRAW)

    # enable array and set up data
    glEnableVertexAttribArray(0)
    glEnableVertexAttribArray(1)
    glEnableVertexAttribArray(2)
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, None)
    # the last parameter is a pointer
    # python donot have pointer, have to using ctypes
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(48))
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(96))
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glBindVertexArray(0)

def render():
    global shaderProgram
    global VAO
    glClearColor(0, 0, 0, 1)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    # active shader program
    glUseProgram(shaderProgram)

    glBindVertexArray(VAO)

    # draw triangle
    glDrawElements(GL_TRIANGLES, 18, GL_UNSIGNED_INT, None)

    glBindVertexArray(0)
    glUseProgram(0)

    glutSwapBuffers()

def main():
    # init freeglut
    glutInit([])

    # make a window
    glutInitContextVersion(3, 3)
    glutInitContextFlags(GLUT_FORWARD_COMPATIBLE)
    glutInitContextProfile(GLUT_CORE_PROFILE)

    glutInitWindowSize(640, 480)
    glutCreateWindow("pyopengl with glut")

    initialize()

    glutDisplayFunc(render)

    glutMainLoop()

if __name__ == '__main__':
    main()