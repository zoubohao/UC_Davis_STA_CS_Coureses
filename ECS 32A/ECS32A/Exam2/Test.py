

def foo(a, b, c):
    sub = a - b
    if sub < c:
        return True
    else:
        return False

def bar1(s):
    for i in range(3, 20, 3):
        print(s[i])
def bar2(s):
    new_s = s[3:20:3]
    for i in new_s:
        print(i)

def goo(string, char):
    count = 0
    for s in string:
        if s == char:
            count += 1
    if 2 <= count <= 4:
        return True
    else:
        return False


if __name__ == "__main__":
    bar1("abcdefghasdfasdfdfecf")
    bar2("abcdefghasdfasdfdfecf")