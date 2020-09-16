
def get_product(lst: list):
    if len(lst) == 0:
        return 1
    else:
        return lst[0] * get_product(lst[1:])


def sum_every_other(lst: list):
    if len(lst) == 0:
        return 0
    elif len(lst) == 1:
        return lst[0]
    else:
        return lst[0] + sum_every_other(lst[2:])

def sum_first_k(lst: list, k: int):
    t = k - 1
    if lst == [] and k == 0:
        return 0
    if k > len(lst):
        raise ValueError("k exceeds len(lst)")
    if t <= 0:
        return lst[0]
    else:
        return lst[0] + sum_first_k(lst[1:],t)

def echo(string: str):
    if len(string) == 1:
        return string[0] + string[0]
    else:
        return string[0] + string[0] + echo(string[1:])

def reverse(vals: list):
    if len(vals) == 0:
        return []
    else:
        return [vals[-1]] + reverse(vals[0:-1])

def foo(val1: list, val2: list):
    if len(val1) != len(val2):
        raise ValueError("The given lists have different lengths.")
    if len(val1) == 0 and len(val2) == 0:
        return True
    elif (val1[0] + val2[0]) != 10:
        return False
    else:
        return foo(val1[1:], val2[1:])




if __name__ == "__main__":
    print(get_product([8,5,2,-1,5]))
    print(sum_every_other([8 , 15 , 20 , -1 , -5 , 4 , 2, -5]))
    print(sum_first_k([7 , 20 , -5 , 14 , 3 , -10 , 0, 5] , 8))
    print(echo("hi there"))
    val = [5, 8, 3]
    print(reverse(val))
    print(val)
    print(foo([5 ,2 , -5] ,[5 ,8]))