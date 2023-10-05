#################
#   GROUP 27    #
#################

# ------- Group Introduction ------- #

# The following members form Group 27:
#   1. Anagha - S2596158
#   2. Anjali - S2580177
#   3. Navya  - S2602689

# Contribution of each member:
#   1. Anagha - Ques 4,5,8 (36% of the total work)
#   2. Anjali - Ques 7,9   (32% of the total work)
#   3. Navya  - Ques 6,10  (32% of the total work)

# We collectively decided this distribution of questions as all 
# of us had to do Ques 1,2 and 3. Ques 5, 9 and 10 seemed easier 
# than the other questions, so each of us picked one. The other
# questions were divided based on number of sub-parts.

# ------- Overview of the project ------- #

# James Joyce's Ulysses is often criticized for appearing random, 
# but a practical can investigate this by generating random text 
# sequences based on word patterns. Using a second-order Markov model, 
# the model generates words sequentially, with each word drawn 
# with a probability dependent on the preceding two words. The 
# model's vocabulary is limited to the m most common words, with 
# m ≈ 1000. Suppose that the m most common words are in a vector b. 
# Let a be the vector of all words in Ulysses. We need to know the 
# probabilities of at being each of the words, bj, in b, 
# given it is in b at all, and that at−1 = bk and at−2 = bi:
#     P(at = bj|at−1 = bk,at−2 = bi,at ∈ b)
# This approach allows for a more accurate comparison of real and 
# random text.

# ------- Ans 1,2,3 ------- #

# We created a repository (called GROUP-27) for this project on github

# Reading the Ulysses plain text file into R
#setwd("Desktop/GROUP-27")
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73)
# Replacing unwanted character strings from the text with no text using
# gsub function, which performs the replacement.
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("


# ------- Ans 4,5 ------- #

split_punct <- function(words, punctuation) {
  
  words.copy <- words
  #Finding the indices of the words that includes the input punctuation
  punct_index <- grep(punctuation, words, fixed=TRUE) 
  #  punct_index
  #  length(punct_index)
  
  #Defining a new emplty/0 vector to store both the words and the punctuations after split
  new_vector <- rep("0",length(words)+length(punct_index)) 
  #  new_vector
  #  length(new_vector)
  
  #finding indices to store the punctuation marks once separated
  punct_vector <- punct_index+1:length(punct_index) ## where should second digit go in xs?
  #  punct_vector
  
  #Creating a copy of the input vector of words
  copy_new_vector <- words.copy
  #  copy_new_vector
  
  #Code to remove the punctuation marks from the words
  #Creating an empty vector to store the updated words
  to_store <- character(length(copy_new_vector))
  #  to_store
  
  # Define the punctuations to remove from the words in the given vector
  chars_to_remove <- c(punctuation)
  
  # Function to remove characters from a word
  remove_characters <- function(word, chars) {
    for (char in chars) {
      word <- gsub(char, "", word, fixed = TRUE)
    }
    return(word)
  }
  # Loop through each word and remove the specified characters
  for (i in 1:length(copy_new_vector)) {
    to_store[i] <- remove_characters(copy_new_vector[i], chars_to_remove)
  }
  
  # Print the new words
  #  print(to_store)
  
  
  new_vector[punct_vector] <- gsub("0", punctuation, new_vector[punct_index])
  #  new_vector
  new_vector[-punct_vector] <- to_store
  # new_vector[-punct_vector]
  
  return(new_vector)
}

a <- split_punct(a, "!")
a <- split_punct(a, ",")
a <- split_punct(a, ".")
a <- split_punct(a, ";")
a <- split_punct(a, ":")
a <- split_punct(a, "?")

# ------- Ans 6 ------- #

#a)
words <-c(a) #pasting value into variable 'words'
words <- c(tolower(words)) #lowercased words
unique_words <- c(unique(words)) ##finding unique words in the text and storing as vector 

#b)
#using lower text vector and unique word vector, to find each corresponding word in both.

indexing <- match(tolower(a), unique_words) #finding vector of indices that indicate which element of the text matches to elements from the unique vectors

#c)
#table(indexing) #counts the number of times unique words come up in table format for ref

# using tabulate function
uword_count<- tabulate(indexing)
#d)
wordfreQ <-table((tolower(a)))

freq_words <- sort(c(uword_count), decreasing = TRUE) #sorting numbers in decreasing to obtain most common to least common number of words
threshold <- freq_words[1000] #obtaining number of times a word  must come in 1000 most common words
cat("Threshold required to retain 1000 words:", threshold, "\n") #printing the threshold value

#e)

m <- 1000 #number of words, given m = 1000
b <- c(names((wordfreQ[uword_count >= threshold][1:m]))) #extracting words associated with the frequencies, with less than or equal to the threshold of 24, iterating from 1 through 1000 elements.

cat("1000 most commonly occurring words: \n", b) #display the required result

# ------- Ans7 ------- #

# Using match function to create a vector giving which element 
# of b each element of the full text vector corresponds to

vec_4 <- match(c(tolower(a)),c(b))

# Creating a matrix with 3 columns such that the first column is
# the index of the most commonly occurring words in a, second 
# column is the index of that vector shifted by one place and the
# third column is the index of that vector shifted by two places.
col_1 <- vec_4
# removing the first entry of the vector so that 
# the following word becomes the starting word
col_2 <- vec_4[-1] 
# removing both the first and the second entries of the vector 
# so that the second following word becomes the starting word
col_3 <- vec_4[-c(1,2)] 

# number of rows in the matrix would be the length of the 
# 3rd column vector, so we will need to delete the last two
# entries of the first column vector and the last entry
# of the second column vector

# deleting the last two entries from first col vector
col_1_T <- head(col_1,-2)
# deleting the last entry from the second col vector
col_2_T <- head(col_2,-1)

# creating a matrix using cbind function, which concatenates the 
# vectors column-wise to form a matrix
# setting deparse level zero to remove column label in output matrix
req_matrix <- cbind(col_1_T,col_2_T,col_3,deparse.level=0)

# Now we need to remove the rows with 1 or more NA values
# We will use rowSums function to check the sum of each row. If there
# are one or more NA values in a row, this function would return NA.
row_sum <- rowSums(req_matrix)
# checking what all rows have NA value using "which" function, which
# returns an index vector which satisfy a given condition.
# Here the condition is checking for NA value. We check for NA values
# using "is.na" function.
na_row_index <- which(is.na(row_sum))
# deleting the na rows to get the matrix T without any NA value. 
T <- req_matrix[-na_row_index,]

# Creating a two column common word pairs matrix, P, 
# using the same ideas.
col_1_P <- head(col_1,-1)
req_matrix_P <- cbind(col_1_P,col_2,deparse.level=0)
row_sum_P <- rowSums(req_matrix_P)
na_row_index_P <- which(is.na(row_sum_P))
P <- req_matrix_P[-na_row_index_P,]
