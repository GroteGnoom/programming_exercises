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
import numpy as np
from OpenGL.GL import *
from OpenGL.GL import shaders
from OpenGL.GLUT import *

VERTEX_SHADER = """
#version 330

in vec4 position;
in vec4 color;
in vec2 texcoord;

out vec4 Color;
out vec2 Texcoord;

void main()
{
    gl_Position = vec4(position.x, position.y, 0,1);
    Color = color;
    Texcoord = texcoord;
}
"""

FRAGMENT_SHADER = """
#version 330

in vec4 Color;
in vec2 Texcoord;

out vec4 outColor;

uniform sampler2D texKitten;
uniform sampler2D texPuppy;

void main()
{
    outColor = mix(texture(texKitten, Texcoord), texture(texPuppy, Texcoord), 0.5);
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

    status=GLint()
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, ctypes.pointer(status))
    print status, GL_TRUE
    
    # triangle position and color
    vertex_data_type=np.float32
    vertex_positions =  [-0.5, 0.5, 0.0, 1, #linksboven: x,y,z,w
                                      0.5, 0.5, 0.0, 1.0,#rechtsboven: x,
                                      0.5, -0.5, 0.0, 1.0,#rechtsonder
                                      -0.5, -0.5, 0.0, 1.0]
    
    vertex_colors =  [1.0, 0.0, 0.0, 1.0, #linksboven RGBA
                                   0.0, 1.0, 0.0, 1.0,#rechtsboven: 
                                   0.0, 0.0, 1.0, 1.0,#rechtsonder
                                  1.0, 1.0, 1.0, 1.0]                                                  
    
    vertex_texture = [0,0, #2d texture coordinaten
                              1,1,
                              0,1,
                              1,0]
    vertexData=np.array(vertex_positions+vertex_colors+vertex_texture,dtype=vertex_data_type)
    # create VAO
    VAO = glGenVertexArrays(1)
    glBindVertexArray(VAO)
    
    #elements
    
    elements=np.array([0,1,2,
                          2,3,0,],dtype=np.uint32)
    
    ebo=glGenBuffers(1)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo)
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, elements.nbytes, elements, GL_STATIC_DRAW)
        
    # create VBO
    VBO = glGenBuffers(1)
    glBindBuffer(GL_ARRAY_BUFFER, VBO)
    glBufferData(GL_ARRAY_BUFFER, vertexData.nbytes, vertexData, GL_STATIC_DRAW)

    # enable array and set up data
    glEnableVertexAttribArray(glGetAttribLocation(shaderProgram, "position"))
    glEnableVertexAttribArray(glGetAttribLocation(shaderProgram, "color"))
    glEnableVertexAttribArray(glGetAttribLocation(shaderProgram, "texcoord"))
    glVertexAttribPointer(glGetAttribLocation(shaderProgram, "position"), 4, GL_FLOAT, GL_FALSE, 0, None)
    # the last parameter is a pointer
    # python donot have pointer, have to using ctypes
    glVertexAttribPointer(glGetAttribLocation(shaderProgram, "color"), 4, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(np.dtype(vertex_data_type).itemsize*len(vertex_positions)))
    glVertexAttribPointer(glGetAttribLocation(shaderProgram, "texcoord"), 2, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(np.dtype(vertex_data_type).itemsize*len(vertex_positions+vertex_colors)))
    #glBindBuffer(GL_ARRAY_BUFFER, 0)
    #glBindVertexArray(0)
    
    #texturethings
    #textures=glGenTextures(2)
    #print textures
    textures=[1,1]
    textures[0]=glGenTextures(1)
    textures[1]=glGenTextures(1)
    """
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    tempcolor=np.array([0.5,0.5,0.5,1],dtype=np.float32)
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR,tempcolor)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glGenerateMipmap(GL_TEXTURE_2D)
    """
    
    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE_2D,textures[0])
    glGenerateMipmap(GL_TEXTURE_2D)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    tempcolor=np.array([0.5,0.5,0.5,1],dtype=np.float32)
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR,tempcolor)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    im=PIL.Image.open("C:/Users/daniel/Documents/python/opengloefenen/sample.png")
    ix, iy, image = im.size[0], im.size[1], im.tostring("raw", "RGB", 0, -1)
    print ix, iy
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, ix, iy, 0, GL_RGB,
              GL_UNSIGNED_BYTE, image)
    print glGetUniformLocation(shaderProgram, "texKitten")
    print glGetError()
    glUniform1i(glGetUniformLocation(shaderProgram, "texKitten"),0)
    
    glActiveTexture(GL_TEXTURE1)
    glBindTexture(GL_TEXTURE_2D,textures[1])
    im=PIL.Image.open("C:/Users/daniel/Documents/python/opengloefenen/sample2.png")
    ix, iy, image = im.size[0], im.size[1], im.tostring("raw", "RGB", 0, -1) 
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, ix, iy, 0, GL_RGB,
              GL_UNSIGNED_BYTE, image)
    glUniform1i(glGetUniformLocation(shaderProgram, "texPuppy"),1)


def render():
    global shaderProgram
    global VAO
    glClearColor(0, 0, 0, 1)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    # active shader program
    glUseProgram(shaderProgram)

    glBindVertexArray(VAO)

    # draw triangle
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, None)
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