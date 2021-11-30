
def adjust(s,n):
    num = ord(s)+n
    if num >ord('z'):
        num-= ord('z')
        num+=ord('a')-1
    return chr(num)
def rot13(s,n):
    res=''
    for i in s.lower():
        res+=adjust(i,n)
    return res
    #pass
print(rot13('cheer',7))



#print(adjust('y',2))