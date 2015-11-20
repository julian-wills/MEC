#Dict-based spectral encoding of given traits based on dictionary word-matching!
#Dominic Burkart

#use: take in all given .txt wordlists in the directory this file is saved infor a given
#   trait (eg emotionality) and encode a given set of tweets with the wordcount from the
#   given dictionary and the ratio of words in the tweet also in the dictionary/total #
#   of words in the tweet.

#for MEC project current version: 19 November 2015 (version 5)
#assumes that words in tweet are separated by spaces/punctuation to allow for tokenization
#no error checking for faulty input. 

inputfiledir = input("data file directory: ")
tw_content_indx = int(input("tweet text index in input file (usually 10): "))

#using standard modules
import csv
import os

#code for cleaning up strings (in dictionaries and in tweets)
punctuation = '''!"#$%&'()*+,-./:;<=>?[\]^_`{|}~'''#missing @ at the request of Julian Wills
def clean(instring, spaces = True): #removes punctuation and double spaces, replacing them w/ single spaces
    if spaces:
        for x in punctuation:
            instring = instring.replace(x, " ")
        while instring.find("  ") > -1:
            instring = instring.replace("  ", " ")
    else:
        for x in punctuation:
            instring = instring.replace(x, " ")
        instring.replace(" ","")
    instring = instring.lower()
    return instring

#gets dictionaries
curlist = os.listdir(os.getcwd())
wordlists = []
listnames = []
i = 0
for fname in curlist:
    if fname.endswith(".txt"):
        wordlists.append(open(fname, encoding = "utf-8").read().splitlines())
        i_of_x = 0
        for x in wordlists[i]:
            wordlists[i][i_of_x] = clean(x, spaces = False)
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
outdoc= csv.writer(open("no_emoji_out.csv", mode = "w", encoding = "utf-8"), lineterminator ="\n")

#takes a line from the in data and encodes it
def findInTweet(line, wordlists):
    content = clean(line[tw_content_indx]).split(" ")
    counts = []
    ratios = []
    for x in range(len(wordlists)):
        counts.append(0) #populates number of variables (eg emotionality)
        ratios.append(0)
    for lists in wordlists:
        for word in lists:
            counts[wordlists.index(lists)] += content.count(word)
    for x in range(len(counts)): #same as len(wordlists)
        ratios[x] = counts[x]/len(content)
    line.extend(counts)
    line.extend(ratios)
    outdoc.writerow(line)
      
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
        

                
