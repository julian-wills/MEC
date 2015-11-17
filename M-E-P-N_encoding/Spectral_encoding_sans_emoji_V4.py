#Dict-based spectral encoding of emotionality, morality, positivity, and negativity!
#Dominic Burkart
#for MEC project current version: 17 November 2015 (version 4)
#assumes that words are separated by spaces to allow for tokenization

#THINGS THAT HAVE TO BE FILLED IN TO MAKE THE PROGRAM WORK:
inputfiledir = "/Users/dominicburkart/Downloads/tweet_in.csv"
tw_content_indx = 10 #index in the input csv that has the actual text of the tweet in each row (eg fourth value in a row of comma-separated values has an index of 3)
morallistdir = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/without_emoji/MoralWords.txt"
emolistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/without_emoji/Affect.txt"
poslistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/without_emoji/pos.txt"
neglistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/without_emoji/neg.txt"

#Great! As long as we run this in Python 3 we should be good!

import csv
import string

indoc = open(inputfiledir, encoding = "utf-8")
outdoc= csv.writer(open("no_emoji_out.csv", mode = "w", encoding = "utf-8"), lineterminator ="\n") #change filename "out.csv" to something else if you want

wordlists  = (open(morallistdir, encoding = "utf-8").read().replace("*", "").replace(" ","").splitlines(),open(emolistdir, encoding = "utf-8").read().replace("*", "").replace(" ","").splitlines(), open(poslistdir, encoding = "utf-8").read().replace("*", "").replace(" ","").splitlines(), open(neglistdir, encoding = "utf-8").read().replace("*", "").replace(" ","").splitlines())
#^update this if you add a list
# opens each file as a list of values
# Storing wordlists as strings in the heap while the program runs is ideal imo.
# todo: next implementation ask for listdirs in a set or list and make this a list + use a while loop to populate it (easier for updating code)
# todo: get lists from cwd

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

def clean(instring): #removes punctuation and double spaces, replacing them w/ single spaces
    for x in string.punctuation:
        instring = instring.replace(x, " ")
    while instring.find("  ") > -1:
        instring = instring.replace("  ", " ")
    return instring       
        
inheader = True 
for line in csv.reader(indoc):
    if inheader: #to copy over header to the new doc + add the new columns :)
        line.extend(["mCount","eCount","pCount","nCount","mRatio","eRatio","pRatio","nRatio"])
        print("populating output file, please wait.")
        outdoc.writerow(line)
        inheader = False
    else: #to count words + ratios for each tweet and then right those values to out :)
        findInTweet(line,wordlists) 
    

        

                
