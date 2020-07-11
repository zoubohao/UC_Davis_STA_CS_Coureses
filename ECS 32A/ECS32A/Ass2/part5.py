
shape_str = input("Enter a shape: ")

if shape_str.lower() == "square":
    length = input("What is the length of the square? ")
    print("The area of the square is {}.".format(int(length)**2))
elif shape_str.lower() == "rectangle":
    hei = int(input("What is the height of the rectangle? "))
    wid = int(input("What is the width of the rectangle? "))
    print("The area of the rectangle is {}.".format(hei * wid))
elif shape_str.lower() == "circle":
    rad = int(input("What is the radius of the circle? "))
    choice = input("Enter 'c' for circumference or 'a' for area: ")
    if choice == "c":
        print("The circumference is {}.".format(rad * 6))
    elif choice == "a":
        print("The area is {}.".format(rad**2 * 3))
    else:
        print("Invalid choice.")
else:print("Invalid shape.")








