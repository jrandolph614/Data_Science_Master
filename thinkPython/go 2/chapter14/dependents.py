def splitter(w):
    w = list(w)
    w.sort()
    res = ''
    res = res.join(w)
    return res
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

def find_anagrams_dict(d):
    res = {}
    for key in d:
        if d[key] not in res:
            res[d[key]] = [key]
        else:
            res[d[key]].append(key)
    return res