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
    gl_Position = model*vec4(position.x, -position.y, 0,1);
    //gl_Position = modelviewproj*vec4(position.x, -position.y, 0,1);
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
    
   
    eye = Vector3(1.2, 1.2, 1.2)
    at = Vector3(0.0, 0.0, 0.0)
    up = Vector3(0.0, 0.0, 1.0)
    view = Matrix4.new_look_at(eye, at, up)
    view = view[:]
    view_ctype = (GLfloat * len(view))(*view)
    uniView = glGetUniformLocation(shaderProgram, "view")
    glUniformMatrix4fv(uniView, 1, GL_FALSE, view_ctype)
    
    proj = Matrix4.new_perspective(math.radians(45.0), 800.0 / 600.0, 1.0, 10.0)
    proj = proj[:]
    proj_ctype = (GLfloat * len(proj))(*proj)
    uniProj = glGetUniformLocation(shaderProgram, "proj")
    glUniformMatrix4fv(uniProj, 1, GL_FALSE, proj_ctype)
    uniModel = glGetUniformLocation(shaderProgram, "model")

   
def rotatex(rad):
    return np.matrix([[1, 0,0,0], [0, np.cos(rad), -np.sin(rad),0],[0, np.sin(rad), np.cos(rad),0],[0,0,0,1]])

def rotatez(rad):
    return np.matrix([[np.cos(rad), -np.sin(rad),0,0],[np.sin(rad), np.cos(rad),0,0],[0, 0,1,0], [0,0,0,1]])
    
def lookat(vec_e, vec_l, vec_U):
    """Vectoren die er in gaan zijn 3 lang. Geefze maar als numpy array. Geeft een 4x4 matrix. e=eye, l=lookat, u=up """
    vec_F=vec_l-vec_e
    vec_f=vec_F/np.linalg.norm(vec_F)
    vec_u_2=vec_U/np.linalg.norm(vec_U)
    vec_s=vec_f*vec_u_2
    vec_u=vec_s*vec_f
    mat_M=np.matrix([[vec_s[0],vec_s[1],vec_s[2],0],[vec_u[0],vec_u[1],vec_u[2],0],[-vec_f[0],-vec_f[1],-vec_f[2],0],[0,0,0,1]])
    mat_T=np.matrix([[1,0,0,-vec_e[0]],[0,1,0,-vec_e[1]], [0,0,1,-vec_e[2]],[0,0,0,1]])
    mat_L=mat_M.dot(mat_T)
    return mat_L
    
def normalize(v):
    norm=np.linalg.norm(v)
    if norm==0: 
       return v
    return v/norm 
   
def lookat2(eye, at, up):
    zaxis=normalize(at-eye)
    xaxis=normalize(up*zaxis)
    yaxis = zaxis*xaxis
    return np.matrix([[xaxis[0], yaxis[0],zaxis[0],0],[xaxis[1], yaxis[1],zaxis[1],0],[xaxis[2], yaxis[2],zaxis[2],0], [-xaxis.dot(eye),-yaxis.dot(eye),-zaxis.dot(eye),1]])
    
def perspective(fov, aspect, near,far):
    uh=1/np.tan(fov/2)
    uw=uh/aspect
    return np.matrix([[uw,0,0,0],[0,uh,0,0],[0,0,far/(far-near),1],[0,0,-far*near/(far-near),0]])
    

    
    
def render():
    global shaderProgram
    global VAO
    glClearColor(0, 0, 0, 1)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    # active shader program
    glUseProgram(shaderProgram)

    #_time=np.float32((time.time()))
	# Calculate transformation
    model = Quaternion.new_rotate_axis(time.clock() * math.pi, Vector3(0, 0, 1))
    model = model.get_matrix()
    model = model[:]
    model_ctype = (GLfloat * len(model))(*model)
    glUniformMatrix4fv(glGetUniformLocation(shaderProgram, "model"), 1, GL_FALSE, model_ctype)
    # Draw a rectangle from the 2 triangles using 6 indices
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, None)
    glutPostRedisplay ()  
    
    
    glBindVertexArray(VAO)

    # draw triangle

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