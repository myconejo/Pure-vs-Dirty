VERBOSE = 0

def hashcode(k, s):
    return k%s

def hIndex(k, t, s):
    return hashcode(hashcode(k,s) + t*hashcode(k,int((s+1)*7)), s)

class Pair:
    def __init__(self, k, v):
        self.key = k
        self.val = v

    def setKey(self, k):
        self.key = k
        return

    def setVal(self, v):
        self.val = v
        return
    
    def getKey(self):
        return self.key
    
    def getVal(self):
        return self.val

    def show(self):
        print("[%d:%d]" % (self.key, self.val))

class Hash:
    def __init__(self, s):
        self.size = s
        self.weight = 0
        self.arr = []
        for i in range(s):
            self.arr.append(None)
       
    def put(self, k, v):
        t = 0
        while (t < self.size*20):
            h = hIndex(k, t, self.size)
            if (self.arr[h] == None):
                self.arr[h] = Pair(k, v)
                self.weight += 1
                if VERBOSE == 1:
                    print("weight is %d" % (self.weight))
                break
            else:
                if (self.arr[h].getKey() == k):
                    print(">> no duplicate keys allowed!")
                    return
                t = t + 1
        if self.weight > int(self.size/2):
            print(">> key (%d) added -> extending the list..." % (k))
            self.extend()
        return
    
    def extend(self):
        newarr = []
        s = self.size
        for i in range(s):
            newarr.append(self.arr[i])
        self.arr = []
        for i in range(s*2):
            self.arr.append(None)
        self.size = 2*s
        self.weight = 0
        for i in range(s):
            if newarr[i] != None:
                self.put(newarr[i].getKey(), newarr[i].getVal())
        return

    def pop(self, k):
        t = 0
        while (t < self.size*20):
            h = hIndex(k, t, self.size)
            if (self.arr[h] != None):
                if (self.arr[h].key == k):
                    self.arr[h] = None
                    self.weight -= 1
                    break
                else:
                    t = t + 1
            else:
                print("!! no such key (%d) found" & (k))
                return
        if int(self.size/2) >= 5:
            if self.weight <= int(self.size/4):
                print(">> diminishing the list...")
                self.diminish()
        return
    
    def diminish(self):
        newarr = []
        s = self.size
        for i in range(s):
            newarr.append(self.arr[i])
        self.arr = []
        for i in range(int(s/2)):
            self.arr.append(None)
        self.size = int(s/2)
        self.weight = 0
        for i in range(s):
            if newarr[i] != None:
                self.put(newarr[i].getKey(), newarr[i].getVal())
        return

    def traverse(self):
        for h in range(self.size):
            if self.arr[h] == None:
                print("[_:_]", end = " ")
            else:
                print("[%d:%d]" % (self.arr[h].key, self.arr[h].val), end = " ")
        print("")
        return
    

h1 = Hash(11)
h1.traverse()
for i in range(9):
    h1.put(3*i,3*i)
h1.traverse()
for i in range(0,8,2):
    h1.pop(3*i)
h1.traverse()
h1.pop(9)
h1.pop(21)
h1.pop(15)
h1.traverse()
