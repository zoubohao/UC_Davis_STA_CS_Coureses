from person import Person
import typing as typing

def remove_vals(vals : typing.List[typing.List[int]], targets : typing.List[int]) -> typing.List[typing.List[int]]:
    """
    The return list should be identical to vals,
    except with any value that appears in targets removed.
    :param vals: A list of lists of integers
    :param targets: A list of integers
    :return:A list of lists of integers
    """
    newVals = []
    for row in vals:
        currentList = []
        for val in row:
            if val not in targets:
                currentList.append(val)
        newVals.append(currentList)
    return newVals


def find_matches(str1: str, str2: str) -> typing.List[int]:
    """
    Find the target indices which appears in matched string.
    :param str1: matched string
    :param str2: target string
    :return: A list of integers containing all non-negative indices
    """
    s2Len = len(str2)
    s1Len = len(str1)
    result = []
    for i in range(0, s1Len):
        if str1[i:i+s2Len] == str2:
            result.append(i)
    return result

def update_sales(curr_sales: typing.Dict, fileName: str) -> typing.Dict:
    """
    :param curr_sales: current sales items dictionary
    :param fileName: The file path
    :return: A new sales items dictionary
    """
    with open(fileName, mode="r") as rh:
        for line in rh:
            oneLine = line.strip("\n").split(" ")
            curLen = len(oneLine)
            if curLen > 2: raise ValueError("Too much information on given line.")
            elif curLen < 2: raise ValueError("Missed information !")
            else:
                key = oneLine[0]
                value = int(oneLine[1])
                if value <= 0: raise ValueError("Item sales must be nonnegative.")
                if key in curr_sales:
                    curr_sales[key] += value
                else:
                    curr_sales[key] = value
    return curr_sales


def read_people(fileName: str):
    """
    :param fileName: The file path
    :return: A dictionary which contains information of each person.
    """
    name2Person = {}
    with open(fileName, mode="r") as rh:
        for line in rh:
            oneLine = line.strip("\n")
            if oneLine != "===":
                if ":" not in oneLine:
                    oneLine = oneLine.split(" ")
                    thisPersonInstance = Person(name=oneLine[0], age=int(oneLine[1]))
                    name2Person[oneLine[0]] = thisPersonInstance
                else:
                    oneLine = oneLine.split(": ")
                    p1 = oneLine[0]
                    p2 = oneLine[1]
                    if p1 not in name2Person.keys() or p2  not in name2Person.keys():
                        raise ValueError("No imaginary friends.")
                    if name2Person[p1].best_friend is None \
                        and name2Person[p2].best_friend is None:
                        name2Person[p1].best_friend = name2Person[p2]
                        name2Person[p2].best_friend = name2Person[p1]
                    else:
                        raise ValueError("No double-crossing or No reaffirming.")
    return name2Person


