
cString = input("Enter string: ")
string = "" + cString

while cString.lower() != "done":
    cString = input("Enter string: ")
    string = cString + string


print(string)




