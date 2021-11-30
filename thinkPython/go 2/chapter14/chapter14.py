import os
'''
for root, dirs, files in os.walk(".", topdown=False):
   for name in files:
      print(os.path.join(root, name))
   for name in dirs:
      print(os.path.join(root, name))'''

from dependents import make_wordict, find_anagrams_dict, splitter
import shelve
d = make_wordict()
d = find_anagrams_dict(d)

def write_dict(d):
    file=  shelve.open('file')
    for key in d:
        file[key] = d[key]
    file.close()
#write_dict(d)

def read_anagrams(s):
    s= splitter(s)
    d= shelve.open('file')
    data = d[s]
    d.close()
    return data

#print(read_anagrams('wake'))
def pipe(cmd):
    fp = os.popen(cmd)
    res = fp.read()
    stat = fp.close()
    assert stat is None
    return res, stat

def compute_checksum(f):
    cmd = 'md5sum ' +f
    return pipe(cmd)

def check_diff(name1, name2):
    cmd = 'diff %s %s' %(name1,name2)
    return pipe(cmd)

import wc
print(wc)