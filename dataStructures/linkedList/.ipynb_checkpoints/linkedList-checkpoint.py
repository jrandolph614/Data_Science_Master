class Node:
    def __init__(self,data):
        self.data = data
        self.next =None 
class linkedList:
    def __init__(self):
        self.head=None
    def printList(self):
        temp = self.head
        while (temp):
            print (temp.data)
            temp =temp.next
    def push(self,ndata):
        new_node = Node(ndata)
        new_node.next = self.head
        self.head = new_node
    def insertAfter(self,prev,ndata):
        if prev is None:
            return
        new_node = Node(ndata)
        new_node.next = prev.next
        prev.next = new_node
    def append(self,ndata):
        new_node = Node(ndata)
        if self.head is None:
            self.head = new_node
            return 
        last = self.head
        while (last.next):
            last = last.next
        last.next = new_node
        
# Code execution starts here
if __name__=='__main__':
 
    # Start with the empty list
    llist = linkedList()
 
    # Insert 6.  So linked list becomes 6->None
    llist.append(6)
 
    # Insert 7 at the beginning. So linked list becomes 7->6->None
    llist.push(7);
 
    # Insert 1 at the beginning. So linked list becomes 1->7->6->None
    llist.push(1);
 
    # Insert 4 at the end. So linked list becomes 1->7->6->4->None
    llist.append(4)
 
    # Insert 8, after 7. So linked list becomes 1 -> 7-> 8-> 6-> 4-> None
    llist.insertAfter(llist.head.next, 8)
 
    print('Created linked list is:'),
    llist.printList()
 
