
def find_highest_in_file(file_path):
    """
    It will return the highest value in the file if there
    only contains one column in the file. else, it will return False
    :param file_path: Input file path
    :return: The highest value or False
    """
    maxNum = None
    with open(file_path, mode= "r") as rh:
        for oneLine in rh:
            try:
                numberValue = int(oneLine)
            except BaseException:
                return False
            if maxNum is None or numberValue > maxNum:
                maxNum = numberValue
    return maxNum

def find_matches(input_file_path, output_path, string, if_contain):
    """
    If the if_contain parameter is True, then this function will return how many
    times that the string parameter will contain in the input file. Then output
    the lines which contain string into output file.
    If the if_contain parameter is False, then the function of this function will
    be reversed according to the description above.
    :param input_file_path: input file path
    :param output_path: output file path
    :param string: the pattern
    :param if_contain: if contain the pattern in input file
    :return: the times which the pattern contains in input file.
    """
    count = 0
    with open(input_file_path, "r") as rh, open(output_path, "w") as wh:
        for oneLine in rh:
            if if_contain:
                if string in oneLine:
                    wh.write(oneLine)
                    count += 1
            else:
                if string not in oneLine:
                    wh.write(oneLine)
                    count += 1
    return count

def draw_entity(entity_ins, gamingBoard):
    """
    :param entity_ins: the instance of entity class
    :param gamingBoard: 2D array
    :return: True if the drawing is successful and False if failed.
    """
    for i in range(entity_ins.top_left_y, entity_ins.top_left_y + entity_ins.height):
        for j in range(entity_ins.top_left_x, entity_ins.top_left_x  + entity_ins.width):
            try:
                gamingBoard[i][j] = entity_ins.icon
            except BaseException:
                return False
    return True

class Entity:
    def __init__(self, icon, top_left_x, top_left_y, width, height):
        self.icon = icon
        self.top_left_x = top_left_x
        self.top_left_y = top_left_y
        self.width = width
        self.height = height





