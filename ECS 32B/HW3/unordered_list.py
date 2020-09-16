# Initial version from section 4.21 of "Problem Solving with Algorithms and Data
# Structures using Python" by Brad Miller and David Ranum.

class Node:
    def __init__(self,initdata):
        self.data = initdata
        self.next = None

    def getData(self):
        return self.data

    def getNext(self):
        return self.next

    def setData(self,newdata):
        self.data = newdata

    def setNext(self,newnext):
        self.next = newnext


class UnorderedList:

    def __init__(self):
        self.head = None
        self.current = None
        self.previous = None


    def isEmpty(self):
        return self.head == None

    def add(self,item):
        temp = Node(item)
        temp.setNext(self.head)
        self.head = temp
        self.current = self.head

    # TODO: Replace this with recursive version.
    def size(self):
        if self.current is not None:
            self.current = self.current.getNext()
            return 1 + self.size()
        else:
            return 0

    def search(self,item):
        current = self.head
        found = False
        while current != None and not found:
            if current.getData() == item:
                found = True
            else:
                current = current.getNext()

        return found

    def _remove(self, item):
        if self.current.getData() != item:
            self.previous = self.current
            self.current = self.current.getNext()
            self._remove(item)
        else:
            if self.previous is None:
                self.head = self.current.getNext()
            else:
                self.previous.setNext(self.current.getNext())

    # TODO: Replace this with recursive version.
    def remove(self,item):
        self.current = self.head
        self._remove(item)
        self.current = self.head
        self.previous = None

    # Added by me, from slide 42 of the linear data structures slide deck.
    # Reluctantly switched to camel case in order to be consistent with
    # the book's convention.
    def printAll(self):
        current = self.head
        while current != None:
            print(current.getData(), end=' ')
            current = current.getNext()
        print()

if __name__ == "__main__":
    mylist = UnorderedList()
    mylist.add(31)
    mylist.add(77)
    mylist.add(17)
    mylist.add(93)
    mylist.add(26)
    mylist.add(54)
    mylist.printAll()
    print(mylist.size())
    mylist.add(100)
    mylist.printAll()
    print(mylist.size())
    mylist.remove(54)
    mylist.printAll()
    print(mylist.size())
    mylist.remove(93)
    mylist.printAll()
    print(mylist.size())
    mylist.remove(31)
    mylist.printAll()
    print(mylist.size())
