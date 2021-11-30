from tkinter.constants import N
from  swampy.TurtleWorld import *
world = TurtleWorld()
def square(bob,l):

    fd(bob,l)
    lt(bob)
    fd(bob,l)
    lt(bob)
    fd(bob,l)
    lt(bob)
    fd(bob,l)
    wait_for_user()
#square(Turtle(),10)


def polygon(bob,l,n):
    d = 360/n
    bob.delay =0.01
    for i in range(n):
        fd(bob,l)
        lt(bob,d)
    wait_for_user()
#polygon(Turtle(),100,8)

import math
def circle(t,r):
    c = 2*math.pi *r
    n = int(c/3)+1
    l = c/n
    polygon(t,l,n)

circle(Turtle(),100)

