#tagCounter
#Dominic Burkart
#for MEC / Van Bavel Lab

import csv

#finds the end of a hashtag
notTag = list(''' !"#$%&'()*+,-./:;<=>?[\]^_`{|}~''')
def final(t):
    last = 1
    while last < len(t):
        if t[last] in notTag:
            return last
        last +=1
    return last

#holds our hashtags
hashtags = {}
def hashtagCheck(line):
    toks = line[tw_content_indx].split(" ")
    for t in toks:
        if t.startswith("#"):
            last = final(t)
            try:
                hashtags[t[1:last]] += 1
            except KeyError:
                hashtags[t[1:last]] = 1

inputfiledir = input("data file directory: ")
tw_content_indx = int(input("tweet text index in input file (usually 10): "))
print("\n")

indoc = open(inputfiledir, encoding = "utf-8")

inheader = True 
for line in csv.reader(indoc):
    if inheader:
        print("Building hashtag dictionary, please wait.")
        inheader = False
    else: 
        hashtagCheck(line)

tagOut = open("hashtagDict.csv", mode = "w", encoding = "utf-8")
hashtagDict = csv.writer(tagOut, lineterminator = "\n")

print("\nSaving hashtag dictionary.")
for h in hashtags:
    line = [h, hashtags[h]]
    hashtagDict.writerow(line)

tagOut.close()
