notes = []
i = 0
# Get input until -1 is entered
while True:
    note = int(input("Enter a number (-1 to stop): "))
    if note == -1: #cutoff
        break
    i += 1
    notes.append(1193180 // note)

# Print the numbers in the array and the length of the array
print("Numbers entered:")
for note in notes:
    print(hex(note).upper())
print("Total notes entered:", i)