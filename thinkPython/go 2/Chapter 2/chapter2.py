#1.
import math
print('1\n',(4/3)*math.pi*5**3)
#2. 
price = 24.95
discount = .4
n = 60
shipping = 3 + .75*(n-1)
cost = price * discount *n+shipping
print('2\n',cost)


#3 
def time_add(h,m):
    pace_1 = minute_calc(8,15) *2
    pace_2 = minute_calc(7,12)*3
    total = pace_1 + pace_2
    minutes = total % 60 



def minute_calc(m,s):
    res = m*60 + s
    return res
