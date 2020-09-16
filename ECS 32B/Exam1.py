
def prod_first_k(vals, k):
    prod = 1
    try:
        for i in range(k):
            prod *= vals[i]
    except:
        print("k is out of index.")
    return prod

def foo(dic:dict):
    num = 0
    for key in dic:
        if key > dic[key]:
            num += 1
    return num




if __name__ == "__main__":
    print(prod_first_k([5,8,2,1,2,10,6],5))
    print(foo({5: 8, 13: -2, 7: 10, 14: 15, 20: 10}))








