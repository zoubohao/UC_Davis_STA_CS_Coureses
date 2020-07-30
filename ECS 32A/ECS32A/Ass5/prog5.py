
def count_in_other(list1, list2):
    """
    This function return the number of elements in the
    second list that are in the first list.
    :param list1: first list
    :param list2: second list
    :return: the number of elements
    """
    count = 0
    for v in list2:
        if v in list1:
            count += 1
    return count

def buy_items(dic1 , dic2 , dic3):
    """
    Simulate buy something at a store
    :param dic1: key is string representing the name of an item, value is the number of that item
    :param dic2: key is string containing the name of an item. value is number of that item that the user is trying to buy
    :param dic3: key is name of item, value is the price
    :return: The total price that the user needs to pay
    """
    price = 0
    ### -1
    for k, v in dic1.items():
        if v <= 0:
            return -1
    for k, v in dic2.items():
        if v <= 0:
            return -1
    for k, v in dic3.items():
        if v <= 0:
            return -1
    ### -2
    for k in dic1:
        if k not in dic3:
            return -2
    ### -3
    for buyItem, number in dic2.items():
        if buyItem not in dic1 or number > dic1[buyItem]:
            return -3
        else:
            price += dic3[buyItem] * number
    return price



def count_less_than(list1 , k ):
    """
    This function will return the number of integers in the first argument that
    are less than k unless at least one of the integers in the first argument is 0.
    It will return -1 in this case.
    :param list1: a double dimensions list
    :param k: a int number
    :return: -1 or the number of elements which are less than k.
    """
    count = 0
    for currentList in list1:
        for c in currentList:
            if c == 0:
                return -1
            if c < k:
                count += 1
    return count


def count_absences(list1 , list2 ):
    """
    This function will count the number of students who are not in their
    proper seat.
    :param list1: seating chart
    :param list2: attendance
    :return: The number of students who are not in their proper seat.
    """
    count = 0
    for i in range(len(list1)):
        for j in range(len(list1[i])):
            if list1[i][j] != list2[i][j]:
                count += 1
    return count

def find_in_other(list1 , list2 ):
    """
    Find the elements in the list1 in list2. This will return a list which
    contains the indices of the element in the list1 that are in list2. Otherwise,
    it will return an empty list.
    :param list1: The first list
    :param list2: The second list
    :return: The indices list
    """
    finalList =  list()
    for v in list1:
        if v not in list2 :
            finalList.append(list())
        else:
            currentList = list()
            for j, s in enumerate(list2):
                if v == s:
                    currentList.append(j)
            finalList.append(currentList)
    return finalList

def count_has_k_even_divisors(n , k ):
    """
    This function return the number of integers between 1 and n (inclusive) that have exactly k positive integers that evenly
    divide them.
    :param n: an integer
    :param k: an integer
    :return: the number of integers
    """
    count = 0
    for i in range(1, n + 1):
        currentCount = 0
        for j in range(1, i + 1):
            if i % j == 0:
                currentCount += 1
        if currentCount == k :
            count += 1
    return count


