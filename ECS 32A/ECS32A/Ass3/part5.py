
string = input("Enter: ")

if len(string) < 3:
    print("The string \"{}\" has less than three characters.".format(string))
elif string[2] == "x":
    print("The third character of \"{}\" is \'x\'.".format(string))
else:
    print("The string \"{}\" has at least three characters.".format(string))






