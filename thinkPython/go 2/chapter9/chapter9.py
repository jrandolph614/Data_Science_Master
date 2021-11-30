#9.1
def overTwenty():
    file = open(r'C:\Users\jrand\Documents\masters\think_python\words.txt')
    for line in file:
        word = line.strip()
        if len(word)>20:
            print(word)
#overTwenty()
import os
def file_open():
    return open(r'C:\Users\jrand\Documents\masters\think_python\words.txt') 
def has_no_e():
    file = file_open()
    e_count = 0
    count = 0
    for line in file:
        word = line.strip()
        if 'e' not in word:
            print(word)
            e_count+=1
        count+=1
    print('percentage of words with no e {}%'.format((e_count/count)*100))
#has_no_e()
def stringer(s,lst):
    for l in lst:
        if l in s:
            return True
    return False
def avoids():
    lst = input('Put letters you would like to avoid')
    lst = lst.split()
    file = file_open()
    for line in file:
        word= line.strip()
        if  stringer(word,lst) == False:
            print(word)
#avoids()
def abecedarian(word):
    for i in range(len(word)-1):
        if word[i]>word[i+1]:
            return False
    return True

def is_abecedarian():
    file = file_open()
    for line in file:
        word= line.strip()
        if abecedarian(word):
            print(word)
#is_abecedarian()


def doubleLetter(s):
    i = 0
    count =0
    while i <len(s)-1:
        if s[i] == s[i+1]:
            count +=1
            if count ==3:
                return True
            i+=2
        else:
            i=i+1-2*count
            count =0
    return False
#print(doubleLetter('ssttqq'))

def puzzler_1():
    file = file_open()
    for line in file:
        word= line.strip()
        if doubleLetter(word):
            print(word)
#puzzler_1()


def is_palindrome(w,l):
    w = str(w)
    if w[-l:] == w[:-l-1:-1]:
        return True
    return False

def puzzler_2():
    for i in range(100000,999999):
        if is_palindrome(i,4):
            if is_palindrome(str(i+1)[1:-1],4):
                if is_palindrome(i+2,6):
                    print(i,i+1,i+2)
#puzzler_2()
def checker(i1,i2):
    if i2<10:
        i2 = int(str(i2).zfill(2)[::-1])

    if str(i1).zfill(2) == str(i2)[::-1].zfill(2):
        return True 
    return False
def puzzler_3():
    m = 36
    while m!=101:
        if checker(m,m-36):
            print(m,m-36)
        m+=1

puzzler_3()
#print(str(40).zfill(2),str(4).zfill(2)[::-1])