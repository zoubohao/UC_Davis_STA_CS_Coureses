from stack import Stack

class Record(object):

    def __init__(self, line_start, left_star, head_star, record_str):
        super(Record, self).__init__()
        self.line_start = line_start
        self.head_star = head_star
        self.left_star = left_star
        self.record_str = record_str


class Editor:
    def __init__(self, infilename, outfilename, settings_filename=None):
        self.window_width = 20
        self.max_window_height = 10

        # NOTE: You cannot change the names of these members. They will be
        # directly checked when the autograder checks load_keys().
        # Default key values:
        self.down_key = 's'
        self.up_key = 'w'
        self.left_key = 'a'
        self.right_key = 'd'
        self.insert_key = 'i'
        self.save_key = 'e'
        self.undo_key = 'u'
        self.redo_key = 'r'


        if settings_filename is not None:
            self.load_keys(settings_filename)
        # TODO: Read contents of input file.
        self.input_content = list()
        self.read_input_file(infilename)
        self.outputFileName = outfilename
        # TODO: Add other members as needed.
        ### head star controls where the head star at, value: [0-19]
        self.headStar = 0
        ### left star controls where the left star at, value: [0-9]
        self.leftStar = 0
        ### line start controls where the first line should start, value: any value
        self.lineStart = 0
        self.undo_history = Stack()
        self.redo_history = Stack()
        self.input_commend = None

    def save(self):
        # TODO: Implement.
        with open(self.outputFileName, "w") as wh:
            for line in self.input_content:
                wh.write(line.rstrip() + "\n")

    def load_keys(self, filename):
        # TODO: Implement.
        with open(filename, "r") as rh:
            for i,line in enumerate(rh):
                oneLine = line.strip("\n")
                if len(oneLine) == 0:
                    raise ValueError("At least one line in settings is empty.")
                elif len(oneLine) > 1:
                    raise ValueError("At least one line in settings is too long.")
                if i == 0:
                    self.down_key = oneLine
                elif i == 1:
                    self.up_key = oneLine
                elif i == 2:
                    self.left_key = oneLine
                elif i == 3:
                    self.right_key = oneLine
                elif i == 4:
                    self.insert_key = oneLine
                elif i == 5:
                    self.save_key = oneLine
                elif i == 6:
                    self.undo_key = oneLine
                elif i == 7:
                    self.redo_key = oneLine

    def move_down(self):
        # TODO: Implement.
        if self.lineStart < (len(self.input_content) - 1):
            if self.leftStar == 9:
                if (self.lineStart + self.leftStar) >= (len(self.input_content) - 1):
                    self.lineStart += 1
                    self.leftStar -= 1
                else:
                    self.lineStart += 1
            else:
                if (self.lineStart + self.leftStar) >= (len(self.input_content) - 1):
                    self.lineStart += 1
                    self.leftStar -= 1
                else:
                    self.leftStar += 1


    def move_up(self):
        # TODO: Implement.
        if (self.leftStar + self.lineStart) > 0:
            if self.leftStar == 0:
                self.lineStart -= 1
            else:
                self.leftStar -= 1


    def move_left(self):
        # TODO: Implement.
        if self.headStar > 0:
            self.headStar -= 1


    def move_right(self):
        # TODO: Implement.
        if self.headStar < 19:
            self.headStar += 1

    def run(self):
        inp = ''
        quit_words = ['q', 'end', 'exit']
        while True:
            self.print_current()
            inp = input("Enter command: ")
            self.input_commend = inp[0:1].rstrip().strip()
            if inp in quit_words:
                break
            elif inp == self.down_key:
                self.move_down()
            elif inp == self.up_key:
                self.move_up()
            elif inp == self.left_key:
                self.move_left()
            elif inp == self.right_key:
                self.move_right()
            elif inp[:2] == self.insert_key + " ":
                to_insert = inp[2:]
                if to_insert == "":
                    print("\nERROR: Need to specify what to insert.")
                else:
                    if not self.insert(to_insert):
                        print("\nERROR: Invalid write.")
            elif inp == self.save_key:
                self.save()
            elif inp == self.undo_key:
                if not self.undo():
                    print("\nERROR: No operation to undo.")
            elif inp == self.redo_key:
                if not self.redo():
                    print("\nERROR: No operation to redo.")
            else:
                print("Invalid command.")
            print()

    def print_current(self):
        # TODO: Implement.
        head = "".join([" " for _ in range(5 + self.headStar)])
        numbers = ""
        for _ in range(2):
            for i in range(1, 11):
                if i != 10:
                    numbers += str(i)
                else:
                    numbers += str(i)[1]
        print(head + "*")
        print("     " + numbers)
        for i in range(self.lineStart, self.lineStart + 10):
            whiteSpace = 4 - len(str(i + 1))
            whiteStr = ""
            if i == (self.lineStart + self.leftStar):
                for _ in range(whiteSpace - 1):
                    whiteStr += " "
                try:print("*" + whiteStr + str(i + 1) + " " + str(self.input_content[i]))
                except: print("*" + whiteStr + str(i + 1) + " ")
            else:
                for _ in range(whiteSpace):
                    whiteStr += " "
                try: print(whiteStr + str(i + 1) + " " + str(self.input_content[i]))
                except: print(whiteStr + str(i + 1) + " ")
        print("     " + numbers)


    def insert(self, to_insert):
        # TODO: Implement.
        ins_len = len(to_insert)
        current_index = self.lineStart + self.leftStar
        current_str = self.input_content[current_index]
        if ins_len > self.window_width:
            return False
        if self.input_commend == self.insert_key:
            oriStr = current_str
            current_str = subIndexString(current_str, to_insert, self.headStar)
            if len(current_str) > self.window_width:
                return False
            current_record = Record(self.lineStart, self.leftStar, self.headStar,
                                    oriStr)
            self.undo_history.push(current_record)
            self.input_content[current_index] = current_str
            emptyStack(self.redo_history)
        else:
            if self.input_commend == self.redo_key:
                current_record = Record(self.lineStart, self.leftStar, self.headStar,
                                        current_str)
                self.undo_history.push(current_record)
            if self.input_commend == self.undo_key:
                current_record = Record(self.lineStart, self.leftStar, self.headStar,
                                        current_str)
                self.redo_history.push(current_record)
            self.input_content[current_index] = to_insert
        return True

    def undo(self):
        # TODO: Implement.
        if self.undo_history.isEmpty():
            return False
        temp_line_start = self.lineStart
        temp_head_star = self.headStar
        temp_left_star = self.leftStar
        record = self.undo_history.pop()
        self.lineStart = record.line_start
        self.headStar = record.head_star
        self.leftStar = record.left_star
        string = record.record_str
        self.insert(string)
        self.lineStart = temp_line_start
        self.headStar = temp_head_star
        self.leftStar = temp_left_star
        return True

    def redo(self):
        # TODO: Implement.
        if self.redo_history.isEmpty():
            return False
        temp_line_start = self.lineStart
        temp_head_star = self.headStar
        temp_left_star = self.leftStar
        record = self.redo_history.pop()
        self.lineStart = record.line_start
        self.headStar = record.head_star
        self.leftStar = record.left_star
        string = record.record_str
        self.insert(string)
        self.lineStart = temp_line_start
        self.headStar = temp_head_star
        self.leftStar = temp_left_star
        return True

    def read_input_file(self, input_file):
        with open(input_file, mode="r") as rh:
            for i, line in enumerate(rh):
                if i >= 30:
                    raise ValueError("The input file has more than 30 lines.")
                oneLine = line.strip("\n")
                if len(oneLine) > self.window_width:
                    raise ValueError("No longer than the window width.")
                self.input_content.append(oneLine)
        if len(self.input_content) == 0:
            raise ValueError("The input file is empty.")


def subIndexString(old_str, sub_str, start_index):
    if start_index <= (len(old_str) - 1):
        return old_str[0:start_index] + sub_str + old_str[start_index + len(sub_str):]
    else:
        return old_str + "".join([" " for _ in range(start_index - len(old_str))]) +\
    sub_str

def emptyStack(stack):
    length = stack.size()
    for _ in range(length): stack.pop()

if __name__ == "__main__":
    e = Editor("Sample Text Files/input4.txt", "ignore")
    e.run()
