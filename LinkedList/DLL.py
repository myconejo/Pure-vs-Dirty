VERBOSE = 1

class Node:
    def __init__(self, k):
        self.key = k
        self.nxt = None
        self.prv = None
        
    def getKey(self):
        return self.key
    
    def getNext(self):
        return self.nxt
    
    def getPrev(self):
        return self.prv
    
    def setKey(self, k):
        self.key = k
    
    def setNext(self, other):
        self.nxt = other
    
    def setPrev(self, other):
        self.prv = other
    
class LinkedList:
    def __init__(self):
        self.head = None
        self.tail = None
        self.size = 0
    
    def enqueue(self, k):
        if (VERBOSE == 1):
            print(">> enqueue %d" % k)
        node = Node(k)
        node.nxt = self.head
        self.size += 1
        if (self.head != None):
            (self.head).setPrev(node)
        else:
            self.tail = node
        self.head = node
        return
    
    def append(self, k):
        if (VERBOSE == 1):
            print(">> append %d" % k)        
        node = Node(k)
        node.prv = self.tail
        self.size += 1
        if (self.head != None):
            (self.tail).setNext(node)
        else:
            self.head = node
        self.tail = node
        return
    
    def dequeue(self):
        if (VERBOSE == 1):
            print(">> dequeue", end = " ")        
        if (self.head == None):
            return
        else:
            self.size -= 1
            if (VERBOSE == 1):
                print((self.head).getKey())            
            ((self.head).getNext()).setPrev(None)
            self.head = (self.head).getNext()
            return

    def pop(self):
        if (VERBOSE == 1):
            print(">> pop", end = " ")           
        if (self.head == None):
            return
        else: 
            self.size -= 1
            if (VERBOSE == 1):
                print((self.tail).getKey())
            ((self.tail).getPrev()).setNext(None)
            self.tail = (self.tail).getPrev()
            return
        
    def traverse(self):
        if (VERBOSE == 1):
            print(">> traversing the linked list")
        walk = self.head
        while(walk != None):
            print("%d" % walk.getKey(), end = " ")
            walk = walk.getNext()
        print("")
        return
        
    def addN(self, n, k):
        if (n >= self.size):
            if (VERBOSE):
                print(">> %d: out of index" % n)
            return
        elif (n == 0):
            self.enqueue(k)
            return
        elif (n == self.size - 1):
            self.append(k)
            return
        else:
            if (VERBOSE):
                print(">> inserting key %d into position %d" % (k, n))
            node = Node(k)
            walk = self.head
            for i in range(0, n):
                walk = walk.getNext()
            node.setNext(walk)
            node.setPrev(walk.getPrev())
            (walk.getPrev()).setNext(node)
            walk.setPrev(node)
            self.size += 1
            return
            
    def delN(self, n):
        if (n >= self.size):
            if (VERBOSE):
                print(">> %d: out of index" % n)            
            return
        elif (n == 0):
            self.dequeue()
            return
        elif (n == self.size - 1):
            self.pop()
            return
        else:
            if (VERBOSE):
                print(">> deleting key at position %d" % n)            
            walk = self.head
            for i in range(0, n):
                walk = walk.getNext()
            (walk.getPrev()).setNext(walk.getNext())
            (walk.getNext()).setPrev(walk.getPrev())
            self.size -= 1
            return
        
    def repN(self, n, k):
        if (n >= self.size):
            if (VERBOSE):
                print(">> %d: out of index" % n)            
            return
        else:
            if (VERBOSE):
                print(">> replacing key of position %d to new key %d" % (n, k))            
            walk = self.head
            for i in range(0, n):
                walk = walk.getNext()
            walk.key = k
            return
        
L1 = LinkedList()
L1.append(3)
L1.append(2)
L1.append(8)
L1.append(6)
L1.append(5)
L1.enqueue(9)
L1.enqueue(4)
L1.append(1)
L1.append(7)
L1.traverse()
print("----------------------------------------------")
L1.pop()
L1.pop()
L1.dequeue()
L1.dequeue()
L1.dequeue()
L1.traverse()
print("----------------------------------------------")
L1.addN(1, 3)
L1.addN(2, 7)
L1.traverse()
print("----------------------------------------------")
L1.delN(0)
L1.delN(2)
L1.repN(3, 4)
L1.traverse()
