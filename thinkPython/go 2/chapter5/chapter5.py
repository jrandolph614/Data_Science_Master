#5.3
def check_fermat():
    a=''
    b=''
    c=''
    n=''
    temp = [a,b,c,n]
    for i in range(len(temp)):
        temp[i] = int(input('Please select {}'.format(temp[i]))) 
    if n>2:
        if a**n + b**n == c**n:
            print('Holy smokes, Fermat was wrong!')
    else:
        print('No, that doesn’t work')
#check_fermat()
def is_triangle(a,b,c):
    if a > b+c:
        print('No')
    if b > a+c:
        print('No')
    if c > b+a:
        print('No')
from swampy.TurtleWorld import *
world = TurtleWorld()
def draw(t, length, n):
    if n == 0:
        return
    angle = 50
    fd(t, length*n)
    lt(t, angle)
    draw(t, length, n-1)
    rt(t, 2*angle)
    draw(t, length, n-1)
    lt(t, angle)
    bk(t, length*n)
draw(Turtle(),10,10)