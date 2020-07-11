import re


fir_str = input("Enter first word: ")
sec_str = input("Enter second word: ")
thi_str = input("Enter third word: ")

str_list = [fir_str, sec_str, thi_str]

regex = re.compile("^blah$")
count = 0
for string in str_list:
    text = regex.match(string)
    if text is not None:
        count += 1

if count == 1:
    print("The word \"blah\" was entered {} time. That's great.".format(count))
else:
    print("The word \"blah\" was entered {} times. That's great.".format(count))




