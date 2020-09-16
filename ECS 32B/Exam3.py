
def sum_below_k(vals: list, k: int):
    if len(vals) == 0 :
        return 0
    elif vals[0] <= k:
        return vals[0] + sum_below_k(vals[1:],k)
    else:
        return 0 + sum_below_k(vals[1:],k)


def find_in_either(s1, s2, target):
    if len(s1) == 0 or len(s2) == 0:
        return 0
    elif s2[0] == target:
        return 2
    elif s1[0] == target:
        return 1
    else:
        return find_in_either(s1[1:], s2[1:], target)


if __name__ == "__main__":
    print(sum_below_k([100, 80, 50, 30, 40, 20, 70], 60))
    print(find_in_either("abc", "def", "x"))




