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
import time
import ctypes
import PIL.Image
import numpy as np
from pprint import pprint
from OpenGL.GL import *
from OpenGL.GL import shaders
from OpenGL.GLUT import *
from euclid import *

VERTEX_SHADER = """
#version 330

in vec4 position;
in vec4 color;
in vec2 texcoord;

out vec4 Color;
out vec2 Texcoord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
//uniform mat4 modelviewproj;


void main()
{
    gl_Position = proj*view*model*vec4(position.x, position.y, 0,1);
    //gl_Position = model*vec4(position.x, -position.y, 0,1);
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
    vec4 colKitten = texture(texKitten, Texcoord);
    vec4 colPuppy = texture(texPuppy, Texcoord);
    outColor = mix(colKitten, colPuppy, 0.5);
}
"""

shaderProgram = None
VAO = None
tex=None
texKitten=None
texPuppy=None

def initialize():
    global VERTEX_SHADER
    global FRAGMENT_SHADER
    global shaderProgram
    global VAO
    global tex
    global texKitten
    global texPuppy
    # compile shaders and program
    vertexShader = shaders.compileShader(VERTEX_SHADER, GL_VERTEX_SHADER)
    fragmentShader = shaders.compileShader(FRAGMENT_SHADER, GL_FRAGMENT_SHADER)
    shaderProgram = shaders.compileProgram(vertexShader, fragmentShader)
    glUseProgram(shaderProgram)
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
                              1,0,
                              1,1,
                              0,1]
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
    
    #texturethings
    textures=glGenTextures(2)
    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE_2D,textures[0])
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    tempcolor=np.array([0.5,0.5,0.5,1],dtype=np.float32)
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR,tempcolor)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glGenerateMipmap(GL_TEXTURE_2D)
    
    im=PIL.Image.open("C:/Users/daniel/Documents/python/opengloefenen/sample.png")
    try:
        ix, iy, image = im.size[0], im.size[1], im.tostring("raw", "RGB", 0, -1) 
    except SystemError: 
        ix, iy, image = im.size[0], im.size[1], im.tostring("raw", "RGBX", 0, -1)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, ix, iy, 0, GL_RGB,
              GL_UNSIGNED_BYTE, image)
    glUniform1i(glGetUniformLocation(shaderProgram, "texKitten"),0)

    
    glActiveTexture(GL_TEXTURE1)
    glBindTexture(GL_TEXTURE_2D,textures[1])
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    tempcolor=np.array([0.5,0.5,0.5,1],dtype=np.float32)
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR,tempcolor)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glGenerateMipmap(GL_TEXTURE_2D)

    im=PIL.Image.open("C:/Users/daniel/Documents/python/opengloefenen/sample2.png")
    try:
        ix, iy, image = im.size[0], im.size[1], im.tostring("raw", "RGB", 0, -1) 
    except SystemError: 
        ix, iy, image = im.size[0], im.size[1], im.tostring("raw", "RGBX", 0, -1)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, ix, iy, 0, GL_RGB,
              GL_UNSIGNED_BYTE, image)
    print glGetUniformLocation(shaderProgram, "texPuppy")
    glUniform1i(glGetUniformLocation(shaderProgram, "texPuppy"),1)
    
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
    glVertexAttribPointer(glGetAttribLocation(shaderProgram, "color"), 4, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(np.dtype(vertex_data_type).itemsize*len(vertex_positions))) #96= number of bytes in 6 vertex vectors
    glVertexAttribPointer(glGetAttribLocation(shaderProgram, "texcoord"), 2, GL_FLOAT, GL_FALSE, 0, ctypes.c_void_p(np.dtype(vertex_data_type).itemsize*len(vertex_positions+vertex_colors)))
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glBindVertexArray(0)
    
    ##
    
    #matrixen en zo
    
    #model=np.matrix(np.identity(4))
    model=np.matrix(np.array([[1.,0.0,0.0,0.0],[0.0,1.0,0.0,0.0],[0.0,0.0,1.0,0.0],[0.0,0.0,0.0,1.0]]))
    model=rotatez(np.pi)*model
    #result=np.array(model).dot([1,0,0,1])

    
    glUniformMatrix4fv(glGetUniformLocation(shaderProgram, "model"), 1, GL_FALSE, np.array(model,dtype=np.float32))
    
    view=np.matrix(np.array([[1.,0.0,0.0,0.0],[0.0,1.0,0.0,0.0],[0.0,0.0,1.0,0.0],[0.0,0.0,0.0,1.0]]))
    eye=np.array([1.2,1.2,1.2])
    at=np.array([0.0,0.0,0.0])
    up=np.array([0.0,0.0,1.0])
    view2=lookat2(eye,at,up)
    view3=lookat(eye,at,up)
    view4 = Matrix4.new_look_at(Vector3(1.2, 1.2, 1.2), Vector3(0.0, 0.0, 0.0), Vector3(0.0, 0.0, 1.0))
    view5=np.matrix(Matrix4.new_look_at(Vector3(1.2, 1.2, 1.2), Vector3(0.0, 0.0, 0.0), Vector3(0.0, 0.0, 1.0))[:]).reshape((4,4)).transpose()
    
    
    glUniformMatrix4fv(glGetUniformLocation(shaderProgram, "view"), 1, GL_FALSE, np.array(view5,dtype=np.float32))
    
    proj=np.matrix(np.array([[1.,0.0,0.0,0.0],[0.0,1.0,0.0,0.0],[0.0,0.0,1.0,0.0],[0.0,0.0,0.0,1.0]]))
    proj2=Matrix4.new_perspective(math.radians(45.0), 800.0 / 600.0, 1.0, 10.0) # uit euclid
    proj3=perspective2(np.pi/4, 4./3., 1., 10.)
    proj4=np.matrix(Matrix4.new_perspective(math.radians(45.0), 800.0 / 600.0, 1.0, 10.0)[:]).reshape((4,4))
    print view4
    print view5
    
    
    glUniformMatrix4fv(glGetUniformLocation(shaderProgram, "proj"), 1, GL_FALSE, np.array(proj3 ,dtype=np.float32))

    
    
    #modelviewproj=view*proj*model
    #modelviewproj=model
    #print modelviewproj2
    #pprint (dir(model))
    #pprint (dir(modelviewproj))
    #glUniformMatrix4fv(glGetUniformLocation(shaderProgram, "modelviewproj"), 1, GL_FALSE, np.array(modelviewproj,dtype=np.float32))
   
def rotatex(rad):
    return np.matrix([[1, 0,0,0], [0, np.cos(rad), -np.sin(rad),0],[0, np.sin(rad), np.cos(rad),0],[0,0,0,1]])

def rotatez(rad):
    return np.matrix([[np.cos(rad), -np.sin(rad),0,0],[np.sin(rad), np.cos(rad),0,0],[0, 0,1,0], [0,0,0,1]])
    
def lookat(eye, center, UP):
    """Vectoren die er in gaan zijn 3 lang. Geefze maar als numpy array. Geeft een 4x4 matrix. e=eye, l=lookat, u=up """
    F=center-eye
    f=normalize(F)
    UPpp=normalize(UP)
    s=np.cross(f,UPpp)
    u=np.cross(normalize(s),f)
    M=np.matrix(np.array([[s[0],s[1],s[2],0],[u[0],u[1],u[2],0],[-f[0],-f[1],-f[2],0],[0,0,0,1]]))
    #mat_T=np.matrix([[1,0,0,-vec_e[0]],[0,1,0,-vec_e[1]], [0,0,1,-vec_e[2]],[0,0,0,1]])
    #mat_L=mat_M.dot(mat_T)
    return M
    
def normalize(v):
    norm=np.linalg.norm(v)
    if norm==0: 
       return v
    return v/norm 
   
def lookat2(eye, at, up):
    zaxis=normalize(at-eye)
    xaxis=normalize(up*zaxis)
    yaxis = zaxis*xaxis
    return np.matrix([[xaxis[0], yaxis[0],zaxis[0],0],[xaxis[1], yaxis[1],zaxis[1],0],[xaxis[2], yaxis[2],zaxis[2],0], [-xaxis.dot(eye),-yaxis.dot(eye),-zaxis.dot(eye),1]]) #.transpose()
    
def perspective(fov, aspect, near,far):
    uh=1/np.tan(fov/2)
    uw=uh/aspect
    return np.matrix([[uw,0,0,0],[0,uh,0,0],[0,0,far/(far-near),1],[0,0,-far*near/(far-near),0]])
    
def perspective2(fov, aspect, near,far):
    f=1/np.tan(fov/2)
    return np.matrix(np.array([[f/aspect,0,0,0],[0,f,0,0],[0,0,(far+near)/(near-far),2*far*near/(near-far)],[0,0,-1,0]]))
    
    
def render():
    global shaderProgram
    global VAO
    glClearColor(0, 0, 0, 1)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    # active shader program
    glUseProgram(shaderProgram)

    #_time=np.float32((time.time()))
    model=rotatez(np.pi*time.time())*np.identity(4)
    glUniformMatrix4fv(glGetUniformLocation(shaderProgram, "model"), 1, GL_FALSE, np.array(model,dtype=np.float32))
    glutPostRedisplay ()  
    
    
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