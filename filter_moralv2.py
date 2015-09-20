import sys
import csv

def lines(fp):
    print str(len(fp.readlines()))

def main():

	#########################################################
	############# Julian Code (mostly obsolete) #############
	#########################################################
	
	print(sys.version)
	dict_file = open(sys.argv[1])
	file_content = dict_file.readlines()	
	#tweet_file = open(sys.argv[2])
	
	dictt = []
	dictTrunc = []
	for line in file_content:
		##add non-asterisk words to dictionary array. If word has asterisk, remove it, then add it. 
		if '*' not in line:
			dictt.append(line.strip()) 
		else: 
			s = line.strip()
			dictTrunc.append(s.replace('*',''))
	
	#print(dictt)
	#################################################
	############# SMAPP Code (Modified) #############
	#################################################
	
	keywords = [line.strip().lower() for line in file_content]
	stems = [kw.strip('*') for kw in keywords if kw.endswith('*')]      
	
	####### 'tweet_in.csv' is just a renamed copy of 'guncontrol_tweets.csv'
	with open('GC_tweets.csv','r') as fin, open('GC_M.csv','wb') as fout1, open('GC_NM.csv','wb') as fout2:
		#writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
		r = csv.DictReader(fin)
		headers = r.fieldnames
		moralwrite = csv.DictWriter(fout1, fieldnames = headers)   
		amoralwrite = csv.DictWriter(fout2, fieldnames = headers) 
		moralwrite.writeheader()
		amoralwrite.writeheader()
		cnt = 0;
		for tweet in r:
			tweet = dict((k, (repr(v) if isinstance(v, float) else str(v))) for k, v in tweet.items())
			tokens = [t.strip('#:;,."\'') for t in tweet['text'].lower().split()]
			cnt += 1
			if cnt%500 == 0:
				print(cnt)
			if any(kw in tokens for kw in dictt) or any(token.startswith(stem) for token in tokens for stem in dictTrunc):
				#print(tweet)
				moralwrite.writerow(tweet)
			else:
				amoralwrite.writerow(tweet)

if __name__ == '__main__':
    main()
