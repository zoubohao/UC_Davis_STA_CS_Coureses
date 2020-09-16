
class Quene:

    def __init__(self):
        self.items = []
        self.start = 0
        self.num = 0

    def enqueue(self, item):
        self.items.append(item)
        self.num += 1

    def dequeue(self):
        if self.num == 0:
            return None
        val = self.items[self.start]
        self.start += 1
        return val

    def size(self):
        return self.num - self.start

def judge(string):
    """
    This function is to judge if the symbol string is balanced.
    balanced string: (), {}, [], (){}[], []{}.
    unbalanced string: (}, ({)}, []((((

    :param string: The input symbol string
    :return: If the symbol string is balanced.
    """
    jNum = 0
    length = len(string) - 1
    for i,s in enumerate(string):
        if s == "(":
            jNum += 1
            if (i + 1) <= length and string[i + 1] != ")":
                return False
        elif s == ")":
            jNum -= 1
        elif s == "[":
            jNum += 2
            if (i + 1) <= length and string[i + 1] != "]":
                return False
        elif s == "]":
            jNum -= 2
        elif s == "{":
            jNum += 3
            if (i + 1) <= length and string[i + 1] != "}":
                return False
        else:
            jNum -= 3
    if jNum == 0:
        return True
    else:
        return False


if __name__ == "__main__":
    testQ = Quene()
    testQ.enqueue(4)
    testQ.enqueue(2)
    testQ.enqueue(3)
    testQ.enqueue(1)
    print(testQ.dequeue())
    print(testQ.size())
    print(testQ.dequeue())

    print(judge("([)]"))

