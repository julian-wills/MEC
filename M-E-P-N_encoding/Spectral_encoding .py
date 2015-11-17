

##CURRENTLY NONFUNCTIONAL. THIS MESSAGE WILL BE DELETED WHEN FUNCTIONALITY IS RESTORED


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
import emoji

indoc = open(inputfiledir, encoding = "utf-8")
outdoc= csv.writer(open("out.csv", mode = "w", encoding = "utf-8")) #change filename "out.csv" to something else if you want

wordlists  = (emoji.demojize(open(morallistdir, encoding = "utf-8").read()).replace("*", "").splitlines(), emoji.demojize(open(emolistdir, encoding = "utf-8").read()).replace("*", "").splitlines(), emoji.demojize(open(poslistdir, encoding = "utf-8").read()).replace("*", "").splitlines(), emoji.demojize(open(neglistdir, encoding = "utf-8").read()).replace("*", "").splitlines(),emoji.demojize(open(emojilistdir, encoding = "utf-8").read()).splitlines())
#^update this if you add a list
# opens each file as a list of values
# Storing wordlists as strings in the heap while the program runs is ideal imo.
# todo: next implementation ask for listdirs in a set or list and make this a list + use a while loop to populate it (easier for updating code)

def findInTweet(line, wordlists):
    content = line[tw_content_indx]
    for x in string.punctuation:
        content = content.replace(x," ")
    for current in wordlists[0:4]: #doesn't go through emojilist
        wordcounts = [0]
        wordratios = [0]
        curIndex   = [0] 
        wordslen   = [0] #for ratios
        for word in current:
            while curIndex[0] < len(content):
            #for words:
                #first word
                if (curIndex[0] == 0 and content.find(word, curIndex[0])> -1):
                    oldi = 0
                    curIndex[0] = endOfWord(content, curIndex[0])
                    wordcounts[0] += 1
                    #print(wordcounts, "first word")
                    wordslen[0] += curIndex[0]-oldi
                #all other words:
                elif (content.find(" "+word, curIndex[0]) > -1):
                    #^ Space added to simulate tokenization: the .find() for the tweet body now functions similarly to the .startswith() for a list of words from the tweet body
                    oldi = curIndex[0] #for getting wordlength
                    curIndex[0] = endOfWord(content, curIndex[0])
                    wordcounts[0] += 1
                    #print(wordcounts, "other words")
                    wordslen[0] += curIndex[0] - oldi
                else:
                    curIndex[0] = endOfWord(content, curIndex[0])
                
    
        wordratios[0] = wordslen[0] / len(content)
        line.append(wordcounts[0])
        line.append(wordratios[0])
    outdoc.writerow(line) #preserves all other data from source doc while appending our values :)


#returns index of the next word for the cleaned tweet line from findInTweet
def endOfWord(instring, curIndex):
    cur = len(instring)
    y = instring.find(" ",curIndex)
    if(y>curIndex and y<cur):
        cur = y
    return cur

for x in wordlists[4]:
    for y in string.punctuation:
        x.replace(y, "")
        
word_ends = list(string.punctuation)
word_ends.append(" ")
inheader = True 
for line in csv.reader(indoc):
    if inheader: #to copy over header to the new doc + add the new columns :)
        line.extend(["mCount","mRatio","eCount","eRatio","pCount","pRatio","nCount","nRatio"])
        print("populating output file, please wait.")
        outdoc.writerow(line)
        inheader = False
    else: #to count words + ratios for each tweet and then right those values to out :)
        findInTweet(line,wordlists) 
    

        

                
