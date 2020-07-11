
def subtract_eight(n):
    return n - 8


def foo(string1, index1, string2, index2):
    if index1 >= len(string1) or index2 >= len(string2):
        return False
    char1 = string1[index1]
    char2 = string2[index2]
    if char1 == char2:
        return True
    else:
        return False


def repeats_same(string):
    k = len(string) - 1
    for i in range(k):
        #print(i)
        if string[i] == string[i + 1]:
            return True
    return False
