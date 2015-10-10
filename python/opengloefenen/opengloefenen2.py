from OpenGL.GL import *
from OpenGL.GL import shaders
from OpenGL.GLU import *
from OpenGL.GLUT import *



def display(w, h):
    aspect = float(w)/float(h)
    glViewport(0, 0, w, h)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-aspect * 5, aspect * 5, -5, 5, -1, 1)

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity()

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    glBegin(GL_QUADS)
    glVertex3f(2,-2,0)
    glVertex3f(2,2,0)
    glVertex3f(-2,2,0)
    glVertex3f(-2,-2,0)
    glEnd()

    glutSwapBuffers()
    
def mypart():
    vertices_py=[0.0,0.5,0.5,-0.5,-0.5,0.5] #to make a triangle
    vertices = (GLfloat * len(vertices_py))(*vertices_py) #changes the list to ctype. I think the star unpacks the elements
    vbo=GLuint() #vertex buffer object = unsigned integer for GL
    glGenBuffers(1, ctypes.pointer(vbo)) #The buffer is generated
    glBindBuffer(GL_ARRAY_BUFFER,vbo) #apparently now it's the active array buffer? 
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW) #Nu hebben we de driehoek naar de buffer gestuurd? de static draw betekend dat ie niet gaat veranderen
    
    # Shaders (Vertex and Fragment shaders) - dit hieronder betekent blijkbaar echt iets
    vertexSource_py = """
    #version 150

    in vec2 position;

    void main()
    {
        gl_Position = vec4(position, 0.0, 1.0);
    }
    """
    fragmentSource_py = """
    #version 150 core
    
    out vec4 outColor;
    
    void main()
    {
        outColor = vec4(0.5, 0.5, 1.0, 1.0);
    }
    """
    
    """
    vertexShader = shaders.compileShader(vertexSource_py, GL_VERTEX_SHADER)
    #glShaderSource=(vertexShader, len(vertexSource_py), ctypes.cast(POINTER(vertexSource), POINTER(vertexSource)), None)
    glShaderSource=(vertexShader, vertexSource_py)
    glCompileShader(vertexShader)

    status=GLint()
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, ctypes.pointer(status))
    print status
   
    fragmentSource = (ctypes.c_char_p * len(fragmentSource_py))(*fragmentSource_py)
    fragmentShader=glCreateShader(GL_FRAGMENT_SHADER) #this makes the fragmentshaders
    #glShaderSource=(fragmentShader, len(vertexSource_py), ctypes.pointer(fragmentSource), None)
    glShaderSource=(fragmentShader, fragmentSource_py)
    glCompileShader(fragmentShader)

    status=GLint()
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, ctypes.pointer(status))
    print status
    
    shaderProgram=glCreateProgram()
    glAttachShader(shaderProgram, vertexShader)
    glAttachShader(shaderProgram, fragmentShader)
    glBindFragDataLocation(shaderProgram, 0, "outColor")
    glLinkProgram(shaderProgram)
    glUseProgram(shaderProgram)
    """
    vertexShader = shaders.compileShader(vertexSource_py, GL_VERTEX_SHADER)
    
    status=GLint()
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, ctypes.pointer(status))
    print status
    
    fragmentShader = shaders.compileShader(fragmentSource_py, GL_FRAGMENT_SHADER)
    
    status=GLint()
    glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, ctypes.pointer(status))
    print status
        
    shaderProgram = shaders.compileProgram(vertexShader, fragmentShader)
    
    status=GLint()
    glGetShaderiv(shaderProgram, GL_COMPILE_STATUS, ctypes.pointer(status))
    print status

    posAttrib = glGetAttribLocation(shaderProgram, "position")
    glVertexAttribPointer(posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0)
    glEnableVertexAttribArray(posAttrib)
    
    vao=GLuint() #vertex buffer object = unsigned integer for GL
    glGenVertexArrays(1, ctypes.pointer(vao))
    glBindVertexArray(vao)
    
    print glGetError()
    
    glDrawArrays(GL_TRIANGLES, 0, 3)

    
def reshape(w, h):
    glutDisplayFunc(lambda: display(w, h))
    glutPostRedisplay();

#if __name__ == '__main__':
if True:
    glutInit()
    glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH)
    glutInitWindowSize(640,480)
    glutCreateWindow("Hello World :'D")

    glutReshapeFunc(reshape)
    glutIdleFunc(glutPostRedisplay)
    mypart()
    glutMainLoop()