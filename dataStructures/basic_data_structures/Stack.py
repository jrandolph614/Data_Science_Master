#Stack Last in First out 


class Stack:
    def __init__(self):
        self.items = []
    def push(self,item):
        self.items.append(item)
    def pop(self):
        self.items.pop()
    def peak(self):
        return self.items[len(self.items)-1]
    def is_empty(self):
        return self.items == []
    def size(self):
        return len(self.items)