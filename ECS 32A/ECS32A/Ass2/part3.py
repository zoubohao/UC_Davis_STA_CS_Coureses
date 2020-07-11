
seconds = int(input("Enter number of seconds: "))
minutes = int(seconds // 60)
reSeconds = seconds % 60
if reSeconds == 1 or minutes == 1:
    print("That is {} minute and {} second.".format(minutes, reSeconds))
else:
    print("That is {} minutes and {} seconds.".format(minutes, reSeconds))






