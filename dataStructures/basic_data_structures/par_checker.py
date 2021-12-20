from Stack import Stack


def par_checker(sybmol_string):
    s = Stack()
    balanced = True
    index = 0
    while index < len(sybmol_string) and balanced:
        sybmol = sybmol_string[index]
        if sybmol == '(':
            s.push(sybmol)
        else: 
            if s.is_empty():
                balanced = False
            else:
                s.pop()

        index +=1
    if balanced == True and s.is_empty():
        return True 
    return False

print(par_checker('((()))'))
print(par_checker('(()'))
