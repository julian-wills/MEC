
#THIS PROGRAM IS CURRENTLY NONFUCTIONAL BECAUSE OF AN ERROR PARSING EMOJI AS INDIVIDUAL TOKENS.
#THIS MESSAGE WILL BE REMOVED WHEN THE ISSUE IS RESOLVED AND THE PROGRAM IS FUNCTIONAL.

#Dict-based spectral encoding of given traits based on dictionary word-matching!
#Dominic Burkart

#use: take in all given .txt wordlists in the directory this file is saved infor a given
#   trait (eg emotionality) and encode a given set of tweets with the wordcount from the
#   given dictionary and the ratio of words in the tweet also in the dictionary/total #
#   of words in the tweet.

#for MEC project current version: 21 November 2015 (version 6)
#assumes that words in tweet are separated by spaces/punctuation to allow for tokenization
#no error checking for faulty input.
#this implementation isn't efficient, but functions within expected applications.

#using standard modules
import csv
import os

#and using emoji (installable from pip)
try:
    import emoji
except ImportError:
    print("Sorry, this program requires the emoji package. Try entering 'pip3 emoji' into your machine's terminal, or however the syntax looks on your OS. Quitting now.")
    quit()

#get filepath for data + content index
inputfiledir = input("data file directory: ")
tw_content_indx = int(input("tweet text index in input file (usually 10): "))
print("\n")

#code for cleaning up strings (in dictionaries and in tweets)
punctuation = '''!"#$%&'()*+,-./:;<=>?[\]^_`{|}~'''#missing @ at the request of Julian
def clean(instring, spaces = True): #removes punctuation and double spaces, replacing them w/ single spaces
    instring.replace("\n"," ")
    for x in punctuation:
            instring = instring.replace(x, " ")
    instring = emoji.demojize(instring) #demojize turns emojis into text with this format: :emoji_text_alias:
    if instring.find(":") > -1: #then the tweet has emojis!
        inlist = instring.split()
        moreEmoji = True
        while moreEmoji:
            try:
                beginning = inlist.index(":")
                end = inlist.index(":",beginning)
                inlist.insert(end+1," ")
            except ValueError:
                moreEmoji = False
        instring = ""
        for x in inlist:
            instring += x
    if spaces:
        while instring.find("  ") > -1: #remove double spaces
            instring = instring.replace("  ", " ")
    else:
        while instring.find(" ") > -1:  #remove all spaces
            instring = instring.replace(" ","")
    instring = instring.lower()
    return instring



#gets dictionaries
curlist = os.listdir(os.getcwd())
temp = []
wordlists = [] #will hold individual words (eg fun)
stemlists = [] #will hold stems (eg funn*)
listnames = [] #will hold the names of keyword files (to be used as variable names)
i = 0 #index of list (
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
if len(listnames) == 0: #we weren't given any lists :(
    print("Input error: please place .txt file wordlists in the same directory as this program before attempting to run it. Thanks! Quitting program.")
    quit()
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
outdoc= csv.writer(open("no_emoji_out.csv", mode = "w", encoding = "utf-8"), lineterminator ="\n")
try:
    indoc = open(inputfiledir, encoding = "utf-8")
except IOError:
    print("Sorry, but I couldn't open the input data file at all. Could you check the filepath and run me again? Quitting now.")
    quit()

#takes a line from the in data and encodes it
def findInTweet(line):
    content = clean(line[tw_content_indx]).split(" ")
    counts = []
    ratios = []
    for x in range(len(wordlists)):
        counts.append(0) #populates number of variables (eg emotionality)
        ratios.append(0)
    for lists in wordlists: #start by grabbing words
        for word in lists:
            counts[wordlists.index(lists)] += content.count(word)
    for lists in stemlists:
        for stem in lists:
            for token in content:
                if token.startswith(stem):
                    counts[stemlists.index(lists)] += 1
    for x in range(len(counts)): #same as len(wordlists)
        ratios[x] = counts[x]/len(content)
    line.extend(counts)
    line.extend(ratios)
    outdoc.writerow(line)
      
#iterates through the input file, calling findInTweet for each row to find and write output.
inheader = True 
for line in csv.reader(indoc):
    if inheader: #to copy over header to the new doc + add the new columns :)
        line.extend(listnames)
        outdoc.writerow(line)
        print("populating output file, please wait.")
        inheader = False
    else: #to count words + ratios for each tweet and then right those values to out :)
        findInTweet(line)
print("\nencoding complete.")
        

                
