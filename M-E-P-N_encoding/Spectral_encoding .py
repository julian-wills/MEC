#Dict-based spectral encoding of emotionality, morality, positivity, and negativity!
#Dominic Burkart
#for MEC project current version: 15 November 2015 (version 2)
#assumes that words are separated by spaces to allow for tokenization

#THINGS THAT HAVE TO BE FILLED IN TO MAKE THE PROGRAM WORK:
inputfiledir = "/Users/dominicburkart/Downloads/tweet_in.csv"
tw_content_indx = 10 #index in the input csv that has the actual text of the tweet in each row (eg fourth value in a row of comma-separated values has an index of 3)
morallistdir = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/MoralWords.txt"
emolistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/Affect.txt"
poslistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/pos.txt"
neglistdir   = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/neg.txt"
emojilistdir = "/Users/dominicburkart/Documents/MEC/M-E-P-N_encoding/MEC_wordlist_dictionaries/emoEmoji.txt"

#make sure that input lists don't have blank lines.
#Great! As long as we run this in Python 3 we should be good!

import csv
import string

indoc = open(inputfiledir, encoding = "utf-8")
outdoc= csv.writer(open("out.csv", mode = "w", encoding = "utf-8")) #change filename "out.csv" to something else if you want

wordlists  = (open(morallistdir, encoding = "utf-8").read().replace("*", "").splitlines(), open(emolistdir, encoding = "utf-8").read().replace("*", "").splitlines(), open(poslistdir, encoding = "utf-8").read().replace("*", "").splitlines(), open(neglistdir, encoding = "utf-8").read().replace("*", "").splitlines(),open(emojilistdir, encoding = "utf-8").read().splitlines())
#^update this if you add a list
# opens each file as a list of values
# Storing wordlists as strings in the heap while the program runs is ideal imo.
# todo: next implementation ask for listdirs in a set or list and make this a list + use a while loop to populate it (easier for updating code)

def findInTweet(line, wordlists): #called by the main program below for each line.
    wordcounts = [0,0,0,0]
    wordratios = [0,0,0,0]
    tweetlength = len(line[tw_content_indx]) #so we can iterate through the list without creating a new object
    for current in range(0, len(wordlists)-1): #doesn't go through emojilist
        curIndex = 0 #we have to iterate through each tweet len(wordlists)-1 number of times with this implementation.
        wordslen = 0 #for ratios
        for word in wordlists[current]:
            #for words:
            if ( curIndex < tweetlength and line[tw_content_indx].find(" "+word, curIndex)> -1):
                #^ Space added to simulate tokenization: the .find() for the tweet body now functions similarly to the .startswith() for a list of words from the tweet body
                oldi = curIndex #for getting wordlength
                curIndex = endOfWord(line[tw_content_indx], curIndex)
                wordcounts[current] += 1
                wordslen += curIndex-oldi
            #for emoji:
            elif ( curIndex < tweetlength and line[tw_content_indx] in emojilistdir and line[tw_content_indx].find(word, curIndex) > -1):
                curIndex += 1
                wordcounts[current] += 1
                wordslen += 1
        wordratios[current] = wordslen / len(line[tw_content_indx])
    line.extend(wordcounts) #adds our counts to the line!
    line.extend(wordratios) #adds our ratios to the line!
    outdoc.writerow(line) #preserves all other data from source doc while appending our values :)

#returns index of the end of the word.
def endOfWord(instring, curIndex): 
    cur = len(instring)-1
    for x in word_ends:
        y = instring.find(x,curIndex)
        if ( y != -1 and y < cur):
            cur = y
    return cur
        

#word_ends will be used to 
word_ends = list(string.punctuation)
word_ends.append(" ")
inheader = True 
for line in csv.reader(indoc):
    if inheader: #to copy over header to the new doc + add the new columns :)
        line.extend(["mCount","eCount","pCount","nCount","mRatio","eRatio","pRatio","nRatio"])
        print("populating output file, please wait.")
        outdoc.writerow(line)
        inheader = False
    else: #to count words + ratios for each tweet and then right those values to out :)
        findInTweet(line,wordlists) 
    

        

                
