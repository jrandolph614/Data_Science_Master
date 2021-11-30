def sumall(*args):
    res = 0
    for i in args:
        res += i
    return res
import random
def sort_by_length(lst):
    t = []
    for w in lst:
        t.append((len(w),random.random(),w))
    t.sort(reverse=True)
    res = []
    for length, _, w in t:
        res.append(w)
    return res
t = sort_by_length(['a','bb','c','d','ee'])

def most_frequent(s):
    d = {}
    for i in s:
        if i not in d:
            d[i] = 1
        else:
            d[i]+=1
    temp =[]
    for i in d:
        temp.append((d[i],i))
    temp.sort(reverse=True)
    for i in temp:
        print(i)
#most_frequent('spoons')
def splitter(w):
    w = list(w)
    w.sort()
    res = ''
    res = res.join(w)
    return res
#print(splitter('spoon'))
def make_wordict():
    d= {}
    file = open(r'C:\Users\jrand\Documents\masters\think_python\words.txt') 
    for line in file:
        word = line.strip().lower()
        sorty = splitter(word)
        d[word]= sorty
    return d

def find_anagrams(d):
    temp = {}
    for key in d:
        if d[key] not in temp:
            temp[d[key]] = [key]
        else:
            temp[d[key]].append(key)
    res = []
    for i in temp:
        res.append((len(temp[i]),temp[i]))
    res.sort(reverse=True)
    final = []
    for i in res:
        final.append(i[1])
    return final

#print(find_anagrams(make_wordict()))
def word_distance(w1,w2):
    assert len(w1) == len(w2)
    count = 0
    for c1,c2 in zip(w1,w2):
        if c1 != c2:
            count+=1
    return count



