def histogram(s):
    d = {}
    for c in s:
        if c not in d:
            d[c]=1
        else:
            d[c]+=1
    return d
s = histogram('spoon')
#print(s.keys())

def sorted_dict(d):
    lst = list(d.keys())
    lst.sort()
    for i in lst:
        print(i,d[i])
#sorted_dict(s)
def reverse_lookup(d,v):
    temp = []
    for k in d:
        if d[k] == v:
            temp.append(k)
    return temp
print(reverse_lookup(s,1))

def invert_dict(d):
    res = {}
    for key in d:
        if d[key] not in res:
            res.setdefault(d[key],[key])
        else:
            res[d[key]].append(key)
    return res
print(invert_dict(s))

known = {0:0,1:1}
def fibonacci(n):
    if n in known:
        return known[n]
    res = fibonacci(n-1)+fibonacci(n-2)
    known[n] = res
    return res
#print(fibonacci(50),known)


c = {}
def ackerman(m,n):
    if m ==0:
        return n+1
    if n==0:
        return ackerman(m-1,1)
    if (m,n) in c:
        return c[m,n]
    else:
        c[m,n] = ackerman(m-1,ackerman(m,n-1))
        return c[m,n]
#print(ackerman(3,4),c)


def has_duplicates(lst):
    d = {}
    for i in lst:
        if i not in d:
            d[i] = i
        else:
            return True
    return False
#print(has_duplicates([1,2,3,4,4]))
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
def make_wordict():
    d= {}
    file = open(r'C:\Users\jrand\Documents\masters\think_python\words.txt') 
    for line in file:
        word = line.strip().lower()
        d[word]= None
    return d

def rotate_pairs(word_dict,word):
    for i in range(1,14):
        rotated = rot13(word,i)
        if rotated in word_dict:
            print(word,i,rotated)
w = make_wordict()
'''for word in w:
    rotate_pairs(w,word)'''

def remove_letters(w):
    w1 = w[1:] 
    w2 = w[0]+w[2:]
    return [w1, w2]

def read_dict():
    d = {}
    file = open(r'C:\Users\jrand\Documents\masters\think_python\c06d.txt')
    for line in file:
        if line[0] == '#':
            continue
        t = line.split()
        word = t[0].lower()
        pron = ' '.join(t[1:])
        d[word]= pron
    return d

def check():
    d = read_dict()

    for word in d:
        r = remove_letters(word)
        if r[0] in d and r[1] in d:
            if d[r[0]] == d[r[1]]:
                print(word,r[0],r[1])
        
check()

#print(remove_letters('word'))