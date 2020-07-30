
def ess(string1 : str, string2 : str):
    """
    Encode the string1 and string2 as single string.
    :param string1: The first string
    :param string2: The second string
    :return: An encoded string.
    """
    dig = len(string1)
    return str(dig) + " " + string1 + string2


def dess(string : str, index: int):
    """
    Decode the string which encodes by ess function.
    :param string: The encoded string
    :param index: Which string should be returned. 1 or 2.
    The other number will be return ERROR
    :return: The first or second string or ERROR
    """
    if index > 2:
        return "ERROR"
    str1Length = int(string[0])
    str1 = string[2:(2 + str1Length)]
    str2 = string[(2 + str1Length):]
    if index == 1:
        return str1
    else:
        return str2

def echo(string: str):
    """
    :param string: One of string.
    :return:A copy of that string with each character duplicated right after it.
    """
    newStr = ""
    for s in string:
        ss = s + s
        newStr += ss
    return newStr

def find_last_mismatch(string1 : str, string2 : str):
    """
     If the strings are identical, then the function will return -1.
     Otherwise, return the last index at which the two strings differ.
    :param string1: The first string
    :param string2: The second string
    :return: -1 or the index
    """
    if string1 == string2:
        return -1
    ### prevent out of index if those strings lengths are not equal.
    minLength = min([len(string1), len(string2)])
    for i in range(minLength - 1, -1, -1):
        if string1[i] != string2[i]:
            return i

def max_length(str1 : str, str2 : str, str3 : str):
    """
    Find the max length of those three strings.
    :param str1: The first string
    :param str2: The second string
    :param str3: The third string
    :return: The length of the largest of the three strings.
    """
    leng1 = len(str1)
    leng2 = len(str2)
    leng3 = len(str3)
    if leng1 >= leng2:
        maxL = leng1
    else:
        maxL = leng2

    if maxL <= leng3:
        return leng3
    else:
        return maxL

def addChar(i, string, newString):
    if i >= len(string):
        return newString + " "
    else:
        return newString + string[i]


def interleave(str1 : str, str2 : str, str3 : str):
    """
    Concat the chars in those strings in order.
    :param str1: The first string
    :param str2: The second string
    :param str3: The third string
    :return: A string that is the result of interleaving the
    given strings together, one character at a time.
    """
    maxL = max_length(str1, str2, str3)
    newString = ""
    for i in range(maxL):
        newString = addChar(i, str1, newString)
        newString = addChar(i, str2, newString)
        newString = addChar(i, str3, newString)
    return newString

def get_longest_stretch(string : str, char : str):
    """
    Find the longest stretch of the target character in the given string.
    An example of this function.
          >>> get_longest_stretch (" effghggefhhhg " , " h")
          >>> 3
    :param string: A string
    :param char: A target character
    :return: The length of the longest stretch of the target character in the given string.
    """
    if char not in string: return 0
    longest = 1
    current = 1
    skip = 0
    for i, s in enumerate(string):
        ### This step is for the situation that if the
        ### string which is long enough and the repeated char is also too long.
        ### This operation will make this program more efficient.
        if i < skip:
            continue
        ### record and update the longest chars
        if s == char:
            w = i + 1
            while w < len(string) and string[w] == char:
                current += 1
                w += 1
            if current != 1 and current > longest:
                longest = current
            current = 1
            skip = w

    return longest

def compute_product_at(list1 : list, list2 : list):
    """
    The product of the values in the first list that are at the indices given in the
    second list.
    :param list1: first values list
    :param list2: second indices list
    :return: The product of those values.
    """
    pro = 1
    for i in list2:
        pro *= list1[i]
    return pro

def insert_name_here(vals : list):
    """
    This function will returns a list containing each integer in vals
    that is greater than the integers immediately before and after it.
    :param vals: values list
    :return: A list which contain integers
    """
    if len(vals) <= 1:
        return list()
    newList = list()
    for i, val in enumerate(vals):
        if i == 0 and val > vals[i + 1]:
            newList.append(val)
        elif i == (len(vals) - 1) and val > vals[i - 1]:
            newList.append(val)
        else:
            if vals[i - 1] < val and val > vals[i + 1]:
                newList.append(val)
    return newList

def get_key_to_min(dic: dict):
    """
    Find the key of the smallest value in the dictionary
    that is greater than or equal to 10.
    :param dic: A dictionary
    :return: The key of the smallest value in the dictionary
    that is greater than or equal to 10.
    """
    minV = 1000
    minKey = None
    for key, value in dic.items():
        if 10 <= value < minV:
            minKey = key
            minV = value
    return minKey










