# You can add methods to this class if you wish.
# The only thing you cannot do is change the names
# of the member variables.
# The argument could be made that Hero and Object should
# be the same class, but I did not do this because:
#   - In my own solution, I have some additional, different
#     methods in each of these two classes.
#   - I thought it might introduce some unnecessary confusion.
class Hero:
    def __init__(self, symbol, start_x, start_y):
        self.symbol = symbol
        self.x = start_x
        self.y = start_y

    # You don't have to use this if you don't want to.
    def draw_on_board(self, board):
        board[self.y][self.x] = self.symbol

# You can add methods to this class if you wish.
# The only thing you cannot do is change the names
# of the member variables.
class Object:
    def __init__(self, symbol, x, y):
        self.symbol = symbol
        self.x = x
        self.y = y

    # You don't have to use this if you don't want to.
    def draw_on_board(self, board):
        board[self.y][self.x] = self.symbol

class Building:
    def __init__(self, x, y):
        self.width = 6
        self.height = 4
        self.door_height = 2
        self.door_width = 2
        self.x = x
        self.y = y

    def draw_on_board(self, board):
        # TODO: Implement.
        for i in range(self.width):
            board[self.y][self.x + i] = "-"
            if i == 2 or i == 3 :
                board[self.y + self.height - 1][self.x + i] = "&"
                board[self.y + self.height - 2][self.x + i] = "&"
            else:
                board[self.y + self.height - 1][self.x + i] = "-"
        for j in range(self.height - 2):
            board[self.y + 1 + j][self.x] = "|"
            board[self.y + 1 + j][self.x + self.width - 1] = "|"



    # Returns True if given the location of this building, the point
    # indicated by (x,y) touches this building in any way.
    def contains(self, x, y):
        # TODO: IMPLEMENT
        startPoint = (self.y, self.x)
        endPoint = (self.y + self.height, self.x + self.width)
        if startPoint[1] <= x <= endPoint[1] and startPoint[0] <= y <= endPoint[0]:
            return True
        else:
            return False

# You don't need to use this function, but I found it useful
# when I wanted to inspect the board from the Interpreter /
# after running the program.
def print_board(board):
    for row in board:
        for spot in row:
            print(spot, end='')
        print()

class Game:
    def __init__(self, input_file_name):
        # You can add member variables to this class.
        # However, you cannot change the names of any of
        # the member variables below; the autograder will
        # expect these member variables to have the names
        # that they have.
        self.hero = Hero(" ",0,0)
        self.num_objects = 0
        self.board = [[None]]
        self.buildings = []
        self.objects = []
        self.down_key = None
        self.up_key = None
        self.right_key = None
        self.left_key = None
        self.objectSym = ""
        ### all the parameters will be initialed in this function
        self.read_input_file(input_file_name)
        self.hero.draw_on_board(self.board)
        self.user_quit = False

    def read_input_file(self, input_file_name):
        # TODO: Implement.
        inputInfor = list()
        with open(input_file_name, "r") as rh:
            for line in rh:
                oneLine = line.strip("\n")
                inputInfor.append(oneLine)
        ### board infor
        boardInfor = inputInfor[0].split(" ")
        self.board = [[" "] * int(boardInfor[0]) for _ in range(int(boardInfor[1]))]
        for i in range(int(boardInfor[0])):
            if i != 0 and i != (int(boardInfor[0]) - 1):
                self.board[0][i] = "-"
                self.board[int(boardInfor[1]) - 1][i] = "-"
        for j in range(int(boardInfor[1])):
            if j != 0 and j != (int(boardInfor[1]) - 1):
                self.board[j][0] = "|"
                self.board[j][int(boardInfor[0]) - 1] = "|"

        ### Hero infor
        heroInfor = inputInfor[1].split(" ")
        self.hero = Hero(symbol=heroInfor[0], start_x = int(heroInfor[1]), start_y=int(heroInfor[2]))
        ### up
        self.up_key = inputInfor[2]
        ### left
        self.left_key = inputInfor[3]
        ### down
        self.down_key = inputInfor[4]
        ### right
        self.right_key = inputInfor[5]
        ### remain
        for i in range(6, len(inputInfor)):
            current = inputInfor[i].split(" ")
            if current[0] == "o":
                self.num_objects += 1
                self.objectSym = current[1]
                self.objects.append(Object(current[1], int(current[2]), int(current[3])))
            if current[0] == "b":
                self.buildings.append(Building(int(current[1]), int(current[2])))
        for build in self.buildings:
            build.draw_on_board(self.board)
        for obj in self.objects:
            obj.draw_on_board(self.board)


    def print_game(self):
        for row in self.board:
            for spot in row:
                print(spot, end='')
            print()

    def game_ended(self):
        return self.all_objects_collected() or self.user_quit

    def all_objects_collected(self):
        return self.num_objects == 0

    def movement(self, inputD):
        if inputD == self.right_key:
            nextHeroPosition = (self.hero.x + 1, self.hero.y)
        elif inputD == self.left_key:
            nextHeroPosition = (self.hero.x - 1, self.hero.y)
        elif inputD == self.up_key:
            nextHeroPosition = (self.hero.x, self.hero.y - 1)
        else :
            nextHeroPosition = (self.hero.x, self.hero.y + 1)
        symbolBoard = self.board[nextHeroPosition[1]][nextHeroPosition[0]]
        if symbolBoard == " ":
            self.board[self.hero.y][self.hero.x] = " "
            self.hero = Hero(self.hero.symbol, nextHeroPosition[0],
                             nextHeroPosition[1])
            self.hero.draw_on_board(self.board)
        if symbolBoard == self.objectSym:
            self.board[self.hero.y][self.hero.x] = " "
            self.hero = Hero(self.hero.symbol, nextHeroPosition[0],
                             nextHeroPosition[1])
            self.hero.draw_on_board(self.board)
            for i,obj in enumerate(self.objects):
                if nextHeroPosition == (obj.x, obj.y):
                    self.num_objects -= 1
                    break

    def run(self):
        # TODO: Finish the implementation. Implement movement
        # and collision detection. You have much flexibity in
        # how you modify this function. For example, you can
        # remove the definitions and usages of the game_ended()
        # and all_objects_collected() methods if you don't want
        # to use those. (My own implementation uses those methods.)
        quit_cmds = ['q', 'end', 'exit']
        while not self.game_ended():
            self.print_game()
            inp = input("Enter: ")
            if inp in quit_cmds:
                self.user_quit = True
            elif inp in [self.up_key, self.down_key, self.left_key, self.right_key]:
                self.movement(inp)
            else:
                print("Invalid command")
        self.print_game()
        if self.user_quit:
            print("You are a quitter!")
        else:
            print("Congratulations: you've collected all of the items!")

# You can have lines like these when you run the program,
# but make sure that they are commented out or removed
# when you submit this file to the autograder.
# g = Game("input1.txt")
# g.run()
# if __name__ == "__main__":
#     b = Building(1,0)
#     board = [[" "] * 10 for _ in range(10)]
#     print(b.contains(10, 20))
#     print(b.contains(2, 1))
#     b.draw_on_board(board)
#     print(board)



