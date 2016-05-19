#Used to take a value assigned to edges and apply it to
# the nodes associated with that edge! NOTE: assumes node
# values are constant across edges and defines node values
# based on first edge occurence.

import csv

inputfiledir = input("data file directory: ")

value = input("value to be applied: ")

indoc = open(inputfiledir, encoding = "utf-8")
out = open("EdgeValtoNodeValOut.csv", mode = "w", encoding = "utf-8")
outdoc = csv.writer(out)

used = []

sourceI = 0
targetI = 0
inheader = True
for line in csv.reader(indoc):
    if inheader:
        sourceI = line.index("Source")
        targetI = line.index("Target")
        valueI = line.index(value)
        inheader = False
    else:
        if line[sourceI] not in used:
            outdoc.writerow([line[sourceI], line[valueI]])
            used.append(line[sourceI])
        if line[targetI] not in used:
            outdoc.writerow([line[targetI], line[valueI]])
            used.append(line[targetI])
out.close()
print("program complete.")
