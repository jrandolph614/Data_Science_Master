import string
def load():
    read = False
    file = open(r'C:\Users\jrand\Documents\masters\think_python\metamorphosis.txt',encoding='utf-8')
    temp = []
    for line in file:
        temp.append(line.split())
    start_stop = []
    for i in range(len(temp)):
        if '***' in temp[i]:
            start_stop.append(i)
    file.close()
    file = open(r'C:\Users\jrand\Documents\masters\think_python\metamorphosis.txt',encoding='utf-8')
    res = []
    for num,line in enumerate(file): 
        if num>min(start_stop) and num<max(start_stop):
                if line not in string.whitespace:
                    line = line.split()
                    for i in range(len(line)):
                        line[i] = line[i].replace(string.punctuation,'').strip(string.punctuation).lower()
                    res.append(line)
    d = {}
    for i in res:
        for q in i:
            if q not in d:
                d[q]=1
            else:
                d[q]+=1
    final = []
    for key in d:
        final.append((d[key],key))
    final.sort(reverse=True)
    for i in range(len(final)):
        if i<20:
            print(final[i])
load()
