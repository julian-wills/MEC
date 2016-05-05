# Author: Danny Vilela
# Date: 14 April 2016
#   This script does a basic extraction of a column within our dataset.
#   The user provides an input file, column name, and output file, and the
#   script attempts to find the specified column within the input file. If found,
#   we print the column's content to the specified output file.

# Get user-provided arguments
args <- commandArgs(trailingOnly = TRUE)

# Less than ideal, but we should check that the user passes in
# at least three arguments --- input file, column, and output file.
if (length(args) < 3) {
  print("Be sure to provide three core arguments: <read_from_file> <column> <output_file>")
  print("You can also specify the following options: verbose (-v, --verbose), append (-a, --append), 
        save output file to input file's directory (-sid, --save-infile-directory)")
  quit(save = "no", status = "-1")
}

# Script option booleans set to FALSE by default
split_sink <- FALSE
append_sink <- FALSE
save_to_infile_dir <- FALSE

# For every optional argument after our core arguments
if (length(args) > 3) {
  
  # Cycle through each argument, and if we have
  # a match on our verbose, append, or output file directory options,
  # set them to true
  for (arg in args[4:length(args)]) {
    if (arg == "-v" | arg == "--verbose")
      split_sink <- TRUE
    if (arg == "-a" | arg == "--append")
      append <- TRUE
    if (arg == "-sid" | arg == "--save-infile-directory")
      save_to_infile_dir <- TRUE
  }
}

# Save the entire path from local to the input file
input_file_directory <- args[1]

# Establish input file, column to be extracted, and output file from :args
input_file <- read.csv(input_file_directory, header = TRUE)
column_to_be_extracted <- as.character(args[2])

# If the user denoted that they want the output file saved
# in the same directory as the input file's directory
if (save_to_infile_dir) {
  
  # First get the output file's name. This'll be used to keep clean.
  output_file_name <- args[3]
  
  # Get the index of our last backslash in :input_file_directory, access the
  # first level. This might look like "5 13 28 41", meaning that there was a
  # backslash at those indices. We reverse that list, so we get "41 28 13 5".
  # Lastly, we access the first index of that list and know where to stop our
  # upcoming substring call.
  last_backslash_index <- rev(gregexpr("\\/", input_file_directory)[[1]])[1]
  
  # Get the string from 1 to :last_backslash_index, within :input_file_directory
  # so the input file "Desktop/Personal/Datasets/Government/2010_spending.csv"
  # would give us "~/Desktop/Personal/Datasets/Government/"
  path_to_input_file <- substring(input_file_directory, 1, last_backslash_index)
  
  # Append our output file name to the input file path, and we're done!
  output_file <- paste(path_to_input_file, output_file_name, sep = "")
} else {
  
  # Otherwise, if we don't want to save our output file in the same directory
  # as the input file, we just assign :output_file to the third argument. This
  # essentially saves the file locally.
  output_file <- args[3]
}

# Determine our :column_to_be_extracted_column's column-based index in :input_file
# e.g. col_index will be assigned 7 if our :column_to_be_extracted is at
#      column index 7 of our :input_file
col_index <- as.integer(which(colnames(input_file) %in% column_to_be_extracted))

# Tell R that we want our file and append options set to our user-provided
# :output_file and :append_sink arguments
sink(file = output_file, append = append_sink)

# Print the output, which really pipes it into the designated :output_file
print(input_file[, col_index])