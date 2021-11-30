class Point:
    """Represents a point in 2-D space"""



class Rectangle:
    """
    Represent a rectangle.
    attributes: width, height, corner
    """

box = Rectangle()
box.width = 100.0
box.height = 200.0
box.corner = Point()
box.corner.x = 0.0
box.corner.y = 0.0

def print_point(p):
    print('(%g, %g)' % (p.x, p.y))
def find_center(rect):
    p=Point()
    p.x = rect.corner.x +rect.width/2
    p.y = rect.corner.y +rect.height/2
    return p
def grow_rectangle(rect,dwidth,dheight):
    rect.width += dwidth
    rect.height += dheight

class Circle:
    '''a circle'''

Circle.center = Point()
Circle.radius = 0.0

Circle.center.x = 150.0
Circle.center.y =100.0
Circle.radius += 75.0


def pointInCircle(circ, pint):
    d = (pint.x - circ.center.x)**2 + (pint.y - circ.center.y)**2
    if d<= circ.radius**2:
        return True 
    
    return False
test = Point()
test.x = 150
test.y = 100
print (pointInCircle(Circle,test))
