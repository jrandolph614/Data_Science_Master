def hypotenuse(a,b):
    h = (a**2+b**2)**(1/2)
    print(h)
    pass
#hypotenuse(3,4)
def is_between(x,y,z):
    return x<=y<=z
#is_between(1,2,3)

def factorial(n):
    if n==0:
        return 1
    else:
        recurse = factorial(n-1)
        result = n*recurse
        return result
(factorial(5))

def fibonacci(n):
    if n == 0:
        return 0 
    elif n ==1:
        return 1
    else:
        return fibonacci(n-1) +fibonacci(n-2)
def ackerman(m,n):
    if m == 0:
        return n+1
    elif m>0 and n==0:
        ackerman(m-1,1)
    elif m>0 and n>0:
        ackerman(m-1,ackerman(m,n-1))

def is_palindrome(s):
    if s == s[::-1]:
        return True
    return False
#print(is_palindrome('soos'))
#print(is_palindrome('froo'))

def is_power(a,b):
    while a%b ==0:
        if a==b:
            return True
        a/=b
    return False
#print(is_power(3,4))
def GCD(a,b):
    if b==0:
        return a
    else:
        return GCD(b,a%b)
print(GCD(21,24))