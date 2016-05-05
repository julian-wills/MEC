import nltk
import sys
import re
import csv

#######
# I/O #
#######

# Argument checking for our required files
if len(sys.argv) < 2:
    print("\nPlease define the corpus (arg 1) and target text (arg 2) paths")
    print("For example, a correct terminal command would be:")
    print("\tpython frequency.py <sub-directory>/<corpus_file>.txt <sub-directory>/<target_file>.txt\n")
    sys.exit(-1)

##############
# Processing
#   Get working directory
#   Get corpus file directory
##############

# Read in our corpus of text data (coerced to lowercase)
# Note: corpus is provided as first command-line argument
corpus_file = open("{}".format(sys.argv[1]), "r")

# We normalize our corpus so that all uppercase letters are converted
# into lowercase letters. This was a decision made to improve signal,
# since "Upright" would not be matched if we were looking for instances
# of "upright". This way, our data is completely neutral in terms of case.
corpus_text = corpus_file.read().lower()

# Read in our target text data
# Note: target file is provided as second command-line argument
target_file = open("{}".format(sys.argv[2]), "r")
target_text = target_file.read()

# Get each word within our target file and corpus tokenized
tokenized_target_text = nltk.word_tokenize(target_text)
tokenized_corpus_text = nltk.word_tokenize(corpus_text)

# Begin by loading each word in :tokenized_target into our dictionary
# such that each word is a key and each word's count is its correlated value
# (initialized to 0)
dictionary = {word: 0 for word in tokenized_target_text}


def last_char_is_wildcard(word):
    """
    Returns boolean depending on whether our last character denotes wildcard

    :param evaluation: word that we're looking at
    :return: boolean (True/False)
    """
    return word[len(word) - 1] == "*"


# TODO consider iterating through the corpus once, incrementing non-wildcard
# TODO words first then pattern-matching the wildcards
def populate_dict_with_word_freq(build_dictionary, corpus, tokenized_corpus):
    """
    Given a dictionary with words in the key fields, we want to populate
    each word's frequency within :corpus and write that value back into
    :build_dictionary

    :param build_dictionary: dict data structure with word : count key-value pairs
    :param corpus: list containing each word from our original corpus
    :param tokenized_corpus: our :corpus param, except pre-tokenized
    :return: populated :build_dictionary dict where :count is no longer 0 for all words
    """

    # Cycle through each word in our dictionary
    for word in build_dictionary.keys():

        # If our word is a wildcard, we treat it differently
        if last_char_is_wildcard(word):

            # We begin by building a new string to represent our intended
            # pattern which if :word is a wildcard, we mean: match the root and
            # any trailing characters until space e.g. for "worth*" we mean
            # "match worthless, worthwhile, worthy, ..."
            # To turn that into a valid regular expression, however,
            # we slice out the "*", and append the raw string
            # that matches:
            #   [\w] the word character class
            #   *    0 or more times
            new_word = word[:len(word) - 1] + r'[\w]*'

            # We compile our regular expression pattern-to-match
            pattern = re.compile(r'{}'.format(new_word))

            # Find all matches for our pattern within :corpus
            all_matches = re.findall(pattern, corpus)

            # The length of :all_matches corresponds to the number of times
            # we found our word, so we set the wildcard's frequency to
            # :all_matches length
            build_dictionary[word] = len(all_matches)

        # If our word is just a word, we utilize the built-in count procedure
        # to return the number of times :word is found within :tokenized_corpus
        else:
            build_dictionary[word] = tokenized_corpus.count(word)

    # Return our populated dictionary
    return build_dictionary


def all_nonzero_words(populated_dictionary):
    """
    Returns a dictionary :nonzero_dictionary that contains all words from
    our param :populated_dictionary that with nonzero frequencies

    :param populated_dictionary: dictionary containing all words, even those
                                 with frequency of 0
    :return: nonzero_dictionary: dictionary containing words with frequency > 0;
                                 subset of :populated_dictionary by definition
    """

    # Initialize our empty dictionary
    nonzero_dictionary = {}

    # Iterate over each word:count pair in :populated_dictionary
    for word, count in populated_dictionary.items():

        # If our :word has been seen more than 0 times
        if count > 0:

            # Add it to :nonzero_dictionary
            nonzero_dictionary[word] = count

    # Return our dictionary containing only words that appear at least once
    return nonzero_dictionary


def in_sorted_order(populated_dictionary):
    """
    Copies our :populated_dictionary to a list of tuples,
    sorted first by frequency then by word (alphabetically)

    :param populated_dictionary: our pre-populated dictionary
                                 of words and counts (frequency)
    :return: a sorted list of tuples where each tuple correlates to one entry
             in :populated_dictionary. This allows us to sort our dictionary
             by frequency, not just alphabetically
    """

    # Convert our dictionary to a list of key, value (or word, count) tuples
    word_and_freq = [(v, k) for k, v in populated_dictionary.items()]

    # Sort our list
    word_and_freq.sort()

    # Invert each tuple to re-orient our list
    word_and_freq = [(k, v) for v, k in word_and_freq]

    # Return our sorted (increasing order) list of tuples
    return word_and_freq


def diagnostic_statistics(populated_dictionary):
    """
    Print more informative information about any word in particular
    within our dictionary :dictionary

    :param populated_dictionary: collection of words we would like to know more about
    """

    print("Number of words in dictionary: {}".format(len(populated_dictionary)))
    print("Number of tokens in corpus: {}".format(len(tokenized_corpus_text)))
    print("Total number of word instances: {}".format(sum(populated_dictionary.values())))

# Populate our dictionary words' frequency
populate_dict_with_word_freq(dictionary, corpus_text, tokenized_corpus_text)

##########
# Output #
##########

output_file_text = open("frequency_result.txt", "w")
output_file_csv = open("frequency_result.csv", "w")

# Initialize our csv writer object to write to our CSV output file
csv_writer = csv.writer(output_file_csv, lineterminator="\n")

# Write the header row for our CSV file only
csv_writer.writerow(["Word", "Frequency"])

# For every word in our list, sorted by count
for word, count in in_sorted_order(dictionary):

    # Write the word, count tuple to the CSV and text files, space-delimited
    output_file_text.write("{0} {1}\n".format(word, count))
    csv_writer.writerow([word, count])

# Print relevant diagnostic statistics relating to our dictionary
diagnostic_statistics(dictionary)

# Close our I/O files
corpus_file.close()
target_file.close()
output_file_text.close()
output_file_csv.close()
