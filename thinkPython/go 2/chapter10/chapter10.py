from typing import Text


def is_sorted(l):
    for i in range(len(l)-1):
        if l[i] > l[i+1]:
            return False 
    return True
def is_anagram(s,s2):
    s= list(s).sort()
    s2=list(s2).sort()
    if s == s2:
        return True
    return False
#print(is_anagram('spoon','noops'))


def has_duplicates(lst):
    temp  = lst[:]
    temp.sort()
    for i in range(len(temp)-1):
        if temp[i] == temp[i+1]: 
            return True
    return False

def remove_duplicates(lst):
    temp = []
    for i in lst:
        if i not in temp:
            temp.append(i)
    return temp
#print(remove_duplicates([1,2,3,3,3,4,5,6,6,7]))

def bisect(lst,elm):
    if len(lst) == 0:
        return False
    i = len(lst)//2
    if lst[i] == elm:
        return True
    if lst[i] > elm:
        return bisect(lst[:i],elm)
    else:
        return bisect(lst[i+1:],elm) 
lst = [1,2,3,4,5,6,7,8,9,10]
#print(bisect(lst,8))
#print(lst[7])

def file_open():
    return open(r'C:\Users\jrand\Documents\masters\think_python\words.txt') 
def word_list():
    file = file_open()
    temp = []
    for line in file:
        temp.append(line.strip())
    return temp
def reverse_pair(word):
    lst = word_list()
    rev_word = word[::-1]
    return bisect(lst,rev_word)
def pairs():
    lst = word_list()
    for word in lst:
        if reverse_pair(word):
            print(word,word[::-1])
#pairs()


def interlock(word,word2):
    word = list(word)
    word2 = list(word2)
    res = ''
    for i in range(len(word)):
        res= res +word[i]
        res= res +word2[i]
    return res

