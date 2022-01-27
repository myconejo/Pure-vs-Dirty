# tree in python
class Tree:
    def __init__(self, k):
        self.left = None
        self.right = None
        self.key = k
        
    def put(self, k):
        subtree = Tree(k)
        if (k == self.key):
            return
        elif (k < self.key):
            if (self.left == None):
                self.left = subtree
                return
            else:
                (self.left).put(k)
        else:
            if (self.right == None):
                self.right = subtree
                return
            else:
                (self.right).put(k)
                
    def find(self, k):
        if (self is None):
            print(">> no key found!")
            return
        elif (k == self.key):
            print(">> key [%d] found!" % self.key)
            return self
        elif (k < self.key):
            if (self.left == None):
                print(">> no key found!")
                return
            (self.left).find(k)
        else:
            if (self.right == None):
                print(">> no key found!")
                return
            (self.right).find(k)
    
    def sel(self):
        return self
    
    def lfm(self):
        if (self.left is None):
            return self
        else:
            return (self.left).lfm()
        
    def suc(self):
        return (self.right).lfm()
    
    def pop(self, k):
        if (self is None):
            return None
        if (k < self.key):
            self.left = self.left.pop(k)
        elif (k > self.key):
            self.right = self.right.pop(k)
        else:
            if (self.left == None):
                temp = self.right
                return temp
            elif (self.right == None):
                temp = self.left
                return temp
            else:
                temp = self.suc()
                self.key = temp.key
                self.right = self.right.pop(temp.key)
        return self;
                

    def traverse(self):
        walk = self
        if (walk.left != None):
            print("(", end = "")
            (walk.left).traverse()
            print(")-", end = "")
        print(walk.key, end = "")
        if (walk.right != None):
            print("-(", end = "")
            (walk.right).traverse()
            print(")", end = "")

T1 = Tree(4)
T1.put(2)
T1.put(6)
T1.put(1)
T1.put(3)
T1.put(5)
T1.put(7)
T1.put(8)
T1.traverse()
print(" ")
T1.pop(7)
T1.pop(8)
T1.pop(4)
T1.traverse()
print(" ")
