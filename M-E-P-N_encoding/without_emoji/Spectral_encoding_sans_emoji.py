#Dict-based spectral encoding of given traits based on dictionary word-matching!
#Dominic Burkart

#use: take in all given .txt wordlists in the directory this file is saved in for a given
#   trait (eg emotionality) and encode a given set of tweets with the wordcount from the
#   given dictionary and the ratio of words in the tweet also in the dictionary/total #
#   of words in the tweet.

#for MEC project current version: 20 November 2015 (version 6)
# -assumes that words in tweet are separated by spaces/punctuation to allow for tokenization
# -no error checking for faulty input. 

#get filepath for data + content index
inputfiledir = input("\ndata file directory: ")
tw_content_indx = int(input("tweet text index in input file (usually 10): "))
print("\n")

#using standard modules
import csv
import os

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
listnames = [] #will hold the names of keyword files (to be used as variable names)
i = 0
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

#creates list of output datafield names based on wordlist file names
temp = []
for x in listnames:
    temp.append(x+"Count")
for x in listnames:
    temp.append(x+"Ratio")
listnames = temp

#removes duplicates
for x in range(len(wordlists)):
    wordlists[x] = set(wordlists[x])

#opens our data and output files
indoc = open(inputfiledir, encoding = "utf-8")
out = open("SpectralEncodingOut.csv", mode = "w", encoding = "utf-8")
outdoc= csv.writer(out, lineterminator ="\n")
tagOut = open("hashtagDict.csv", mode = "w", encoding = "utf-8")
hashtagDict = csv.writer(tagOut, lineterminator = "\n")
        

#takes a line from the in data and encodes it
def findInTweet(line, wordlists):
    hashtagCheck(line)
    content = clean(line[tw_content_indx]).split(" ")
    counts = []
    ratios = []
    for x in range(len(wordlists)):
        counts.append(0) #populates number of variables (eg emotionality)
        ratios.append(0)
    for lists in wordlists: #start by grabbing words
        for word in lists:
            counts[wordlists.index(lists)] += content.count(word)
    for lists in stemlists: #then grab stems
        for stem in lists:
            for token in content:
                if token.startswith(stem):
                    counts[stemlists.index(lists)] += 1
    for x in range(len(counts)): #same as len(wordlists)
        ratios[x] = counts[x]/len(content)
    line.extend(counts)
    line.extend(ratios)
    outdoc.writerow(line)

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

#finds the end of a hashtag for hashtagCheck
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
      
#iterates through the input file, calling the methods to find and write output.
inheader = True 
for line in csv.reader(indoc):
    if inheader: #to copy over header to the new doc + add the new columns :)
        line.extend(listnames)
        outdoc.writerow(line)
        print("populating output file, please wait.")
        inheader = False
    else: #to count words + ratios for each tweet and then right those values to out :)
        findInTweet(line,wordlists)
print("\nencoding complete.")

print("\nSaving hashtag dictionary.")
for h in hashtags:
    line = [h, hashtags[h]]
    hashtagDict.writerow(line)

indoc.close()
out.close()
tagOut.close()

print("\n Program complete.")


        

                
