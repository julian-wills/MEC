import csv
import os
from afinn import *

inputfiledir = input("data file directory: ")
tw_content_indx = int(input("tweet text index in input csv (usually 6): "))
print("\n")

listnames = ["union words", "union wordcount", "union AFINN score", "union AFINN valence"]

afinn = Afinn()
print("AFINN successfully imported.")

indoc = open(inputfiledir, encoding = "utf-8")
out = open("Union_Found.csv", mode = "w", encoding = "utf-8")
outdoc = csv.writer(out)

#code for cleaning up strings (in dictionaries and in tweets)
punctuation = '''!"#$%&'()*+,-./:;<=>?[\]^_`{|}~'''#missing @ at the request of Julian
def clean(instring, spaces = True): #removes punctuation and double spaces, replacing them w/ single spaces
    instring.replace("\n"," ")
    for x in punctuation:
            instring = instring.replace(x, " ")
    if spaces:
        while instring.find("  ") > -1:
            instring = instring.replace("  ", " ")
    else:
        while instring.find(" ") > -1:
            instring = instring.replace(" ","")
    instring = instring.lower()
    return instring

#gets dictionaries
curlist = os.listdir(os.getcwd())
temp = []
wordlists = [] #will hold individual words (eg fun)
stemlists = [] #will hold stems (eg funn*)
listnames = [] #will hold the names of keyword files
i = 0
print("\ngetting dictionaries for LUKE")
for fname in curlist:
    if fname.endswith(".txt"): #new list of keywords!
        wordlists.append([])
        stemlists.append([])
        temp.append(open(fname, encoding = "utf-8").read().splitlines())
        i_of_x = 0
        for x in temp[i]:
            if temp[i][i_of_x].find("*") > -1:
                stemlists[i].append(clean(temp[i][i_of_x], spaces = False))
            else:
                wordlists[i].append(clean(temp[i][i_of_x], spaces = False))
            i_of_x += 1
        uncheckedSpace = True
        uncheckedBlank = True
        while uncheckedSpace or uncheckedBlank:
            try:
                wordlists[i].remove(" ")
            except ValueError:
                uncheckedSpace = False
            try:
                wordlists[i].remove("")
            except ValueError:
                uncheckedBlank = False
        print("Imported dictionary: "+fname)
        i += 1
        listnames.append(fname.split(".")[0])
print("\n")

#removes duplicates in wordlists
for x in range(len(wordlists)):
    wordlists[x] = set(wordlists[x])

#takes a line from the in data and encodes it
def showTokens(line):
    content = clean(line[tw_content_indx]).split(" ")
    words = []
    for lists in wordlists: #start by grabbing words
        for word in lists:
            if word in content:
                words.append(word)
    for lists in stemlists: #then grab stems
        for stem in lists:
            for token in content:
                if token.startswith(stem):
                    words.append(token)
    return words


def unionChecker(line):
    
    shared = []

    content_string = clean(line[tw_content_indx])
    
    #first get matched tokens for AFINN
    a_matched = afinn.find_all(line[tw_content_indx])
    
    #then get matched tokens for LUKE
    l_matched = showTokens(line)

    for a in a_matched:
        if a in l_matched:
            shared.append(a)

    wordcount = len(shared)
    
    #amount of AFINN score that's overlap
    overlap = 0
    valence = 0
    for word in shared:
        a_scores = afinn.score(word)
        valence += a_scores[2]
        if a_scores[0] > 0:
            overlap += a_scores[0]
        if a_scores[1] >0:
            overlap -= a_scores[1]

    #for csv printing the list of shared words
    union = ""
    for s in shared:
        union = union + s + " "
    
    return [union, wordcount, overlap, valence]

#iterates through the input file and calls encode for each line:
inheader = True 
for line in csv.reader(indoc):
    if inheader: 
        line.extend(listnames)
        outdoc.writerow(line)
        print("populating output file, please wait.")
        inheader = False
    else:
        union = unionChecker(line)
        line.extend(union)
        outdoc.writerow(line)
        

out.close()
print("\nunions found. program complete.")
