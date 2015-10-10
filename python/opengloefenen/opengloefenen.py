from OpenGL.GL import *
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
    glutMainLoop()