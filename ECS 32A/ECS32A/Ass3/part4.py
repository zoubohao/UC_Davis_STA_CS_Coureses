
sign = True
qTimes = 0
while sign:
    string = input("Enter: ")
    if string == "Done":
        sign = False
    elif string == "q":
        qTimes += 1
    else:
        if qTimes >= 4:
            print("Enjoy your soda!")
            sign = False
        else:
            print("You must enter {} more quarters.".format(4 - qTimes))










