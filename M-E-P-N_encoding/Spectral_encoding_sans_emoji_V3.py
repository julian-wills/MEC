#Dict-based spectral encoding of emotionality, morality, positivity, and negativity!
#Dominic Burkart
#for MEC project current version: 17 November 2015 (version 3)
#assumes that words are separated by spaces to allow for tokenization

#THINGS THAT HAVE TO BE FILLED IN TO MAKE THE PROGRAM WORK:
inputfiledir = "/Users/dominicburkart/Downloads/tweet_in.csv"
tw_content_indx = 10 #index in the input csv that has the actual text of the tweet in each row (eg fourth value in a row of comma-separated values has an index of 3)
morallistdir = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/without_emoji/MoralWords.txt"
emolistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/without_emoji/Affect.txt"
poslistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/without_emoji/pos.txt"
neglistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/without_emoji/neg.txt"

#make sure that input lists don't have blank lines.
#Great! As long as we run this in Python 3 we should be good!

import csv
import string
import os

indoc = open(inputfiledir, encoding = "utf-8")
outdoc= csv.writer(open("no_emoji_out.csv", mode = "w", encoding = "utf-8"), lineterminator ="\n") #change filename "out.csv" to something else if you want

wordlists  = (open(morallistdir, encoding = "utf-8").read().replace("*", "").replace(" ","").splitlines(),open(emolistdir, encoding = "utf-8").read().replace("*", "").replace(" ","").splitlines(), open(poslistdir, encoding = "utf-8").read().replace("*", "").replace(" ","").splitlines(), open(neglistdir, encoding = "utf-8").read().replace("*", "").replace(" ","").splitlines())
#^update this if you add a list
# opens each file as a list of values
# Storing wordlists as strings in the heap while the program runs is ideal imo.
# todo: next implementation ask for listdirs in a set or list and make this a list + use a while loop to populate it (easier for updating code)



def findInTweet(line, wordlists):
    content = line[tw_content_indx]
    content = " "+content #fixes first word problem
    for x in string.punctuation:
        content = content.replace(x," ")
    content = content.lower()
    for current in wordlists:
        wordcounts = 0
        wordratios = 0 
        wordslen   = 0 #for finding ratios
        for word in current:
            tup = iterForWord(content, word)
            wordcounts += tup[0]
            wordslen += tup[1] -1 #deletes the last space
        wordratios = wordslen / len(content.replace(" ",""))
        line.append(wordcounts)
        line.append(wordratios)
    outdoc.writerow(line) #preserves all other data from source doc while appending our values :)


#returns index of the next word for the cleaned tweet line from findInTweet
def endOfWord(content, curIndex):
    cur = len(content)
    y = content.find(" ",curIndex)
    if(y>curIndex and y<cur):
        cur = y
    return cur

def iterForWord(content, word): #fixes same word not counted twice problem
    inst_wordcounts = 0
    inst_wordslen = 0
    curIndex = 0
    while curIndex < len(content):
    #for words:
        if (content.find(" "+word, curIndex) > -1):
            oldi = curIndex #for getting wordlength
            curIndex = endOfWord(content, curIndex)
            inst_wordcounts += 1
            inst_wordslen += curIndex - oldi
        else:
            curIndex = endOfWord(content, curIndex)
        if (curIndex == len(content)):
            return (inst_wordcounts, inst_wordslen)
        
        
inheader = True 
for line in csv.reader(indoc):
    if inheader: #to copy over header to the new doc + add the new columns :)
        line.extend(["mCount","mRatio","eCount","eRatio","pCount","pRatio","nCount","nRatio"])
        print("populating output file, please wait.")
        outdoc.writerow(line)
        inheader = False
    else: #to count words + ratios for each tweet and then right those values to out :)
        findInTweet(line,wordlists) 
    

        

                
