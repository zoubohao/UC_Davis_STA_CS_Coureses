
target = int(input("Enter target number: "))
start = int(input("Enter start of range: "))
end = int(input("Enter end of range: "))

left = target - start
right = end - target

if 0 <= left and 0 < right:
    print("Within range!")
else:
    print("Not within range...")




