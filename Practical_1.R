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
a <- gsub("_","",a,fixed=TRUE) ## remove "_"
a <- gsub("(","",a,fixed=TRUE) ## remove "("
a <- gsub("_)","",a,fixed=TRUE) ## remove "_)"
a <- gsub(")","",a,fixed=TRUE) ## remove ")"

# ------- Ans 4,5 ------- #

#Anagha's part for Practical-1(I)

#Creating split_punct() AND Separating punctuation marks and words

#Question 4: Creating split_punct()
#Defining a split_punct() function that takes a vector of words and a punctuation as arguments
split_punct <- function(words, punctuation) {
  
  #Finding the indices of the words that includes the input punctuation using grep()
  punct_index <- grep(punctuation, words, fixed=TRUE) 
  
  #Defining a new empty/0 vector to store both the words and the punctuations after split
  new_words <- rep("0",length(words)+length(punct_index)) 
  
  #Finding indices to store the punctuation marks once separated
  punct_vector <- punct_index+1:length(punct_index) 
  
  #Creating a copy of the input vector of words
  copy_new_vector <- words
  
  #Code to remove the punctuation marks from the words
  
  #Creating an empty vector to store the updated words
  to_store_updated <- character(length(copy_new_vector))
  
  #Define the punctuations to remove from the words in the given vector
  to_remove <- c(punctuation)
  
  # Function to remove punctuations from a word using gsub() by looping
  remove_punct <- function(word, chars) {
    for (char in chars) {
      word <- gsub(char, "", word, fixed = TRUE)
    }
    return(word)
  }
  
  # Using remove_punct(), loop through each word and remove the specified punctuations
  for (i in 1:length(copy_new_vector)) {
    to_store_updated[i] <- remove_punct(copy_new_vector[i], to_remove)
  }
  
  #Adding the punctuation to the indices of the new word vector
  new_words[punct_vector] <- gsub("0", punctuation, new_words[punct_index])
  #Storing the updated words into the new vector
  new_words[-punct_vector] <- to_store_updated
  
  return(new_words)
}

#Question 5: Separating punctuation marks and words using split_punct()
a <- split_punct(a, "!")
a <- split_punct(a, ",")
a <- split_punct(a, ".")
a <- split_punct(a, ";")
a <- split_punct(a, ":")
a <- split_punct(a, "?")
a <- split_punct(a, "-")
a <- split_punct(a, "—")


# ------- Ans 6 ------- #
#Navya's part for Practical-1

#ANS Q6

#a)

#storing vector of words after processing into variable named 'words'.
words <-c(a)
#converting that vector 'words' into all-lower cases and updating that variable itself.
words <- c(tolower(a)) 
#finding vector of unique words and storing it in a variable named "unique_words".
unique_words <- c(unique(words)) 

#b)

#Finding vector of indices indicating which element in the 
#unique word vector each element in the lower case text corresponds to, and storing the result in variable named "indexing".
indexing <- match(words, unique_words)
#checking the length of the index vector and the text vector'a'.
check_length <- length(indexing) == length(a) #value return TRUE if same length and FALSE otherwise
#print the findings as BOOLEAN value. 
print(check_length) #value return TRUE if same length and FALSE otherwise
#The lengths are same, hence verified.

#c)

#Using 'tabulate' to count how many times each unique word occurs in the text with 'indexing'
uword_count<- tabulate(c(indexing))

#d)

#finding frequencies of the words in vector of 'words'(which is lower-cased)
word_freq <- table(c(words))
#sorting from decreasing order to have most occurring words towards the start of the list
freq_words <- sort(c(uword_count), decreasing = TRUE)

#given that m ≈ 1000;
m <- 1000 
#finding threshold for 1000 words from the above-'freq_words'
threshold <- freq_words[m] 
#Displaying the threshold value
cat("Threshold required to retain m ≈ 1000 most common words:", threshold, "\n") 

#e)

#given that m ≈ 1000;
m <- 1000 
#Most occurring words can be included in the set of m ≈ 1000 most common words,
#if the frequency of that word is equal to or more than the threshold value found
#and storing those words as vector in 'b'.
b <- c(names((word_freq[uword_count >= threshold][1:m])))

# ------- Ans7 ------- #
# Anjali's part for Practical-1(I)

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

# ------- Ans8 ------- #
#Anagha's part for Practical-1(II)

##Question 8: To simulate 50-word sections from the model

#Function to generate a 50-word section

#defining the function simu_fn() to generate 50 words
simu_fn <- function(words, T, P) {
  
  #Initializing an empty character vector to store a word
  char <- character(0)  
  
  #Starting with two initial words using sample()
  first_word <- sample(words, 1)
  second_word <- sample(words, 1)
  
  #Creating the loop for generating 50 words
  for (i in 1:50) {
    # Extracting sub-matrix by finding rows in T where first_word is in the first column and second_word is in the second column
    #using which() function by checking their indices
    find_rows <- which(T[, 1] == first_word & T[, 2] == second_word)
    
    if (length(find_rows) > 0) {
      # Randomly selecting one of the matching rows
      select_row <- sample(find_rows, 1)
      
      #Getting the next word from the third column of the selected row
      next_word <- words[T[select_row, 3]]
    } else {
      # If no matching rows in T, checking P
      match_in_p <- which(P[, 1] == first_word)
      if (length(match_in_p) > 0) {
        # Randomly selecting a row from P and getting the second word as the next word
        select_row <- sample(match_in_p, 1)
        next_word <- words[P[select_row, 2]]
      } else {
        # If no matching pairs in P, choose a common word randomly
        next_word <- sample(words, 1)
      }
    }
    
    # Adding the next word to the total text as a vector
    char <- c(char, next_word)
    
    # Updating first_word and second_word for the next iteration
    first_word <- second_word
    second_word <- next_word
  }
  
  # Returning the generated text as a single string
  return(paste(char, collapse = " "))
}

# Defining a 50-word section as para
# take sample from b for 50 words
para <- simu_fn(b, T, P)

# Printing the generated section with cat
cat(para, "\n")

# ------- Ans 9 --------- #
# Anjali's part for Practical-1(II)

# Calculating the probability of a word in b
# Indexing vector b from the set of all words
index_b <- match(words, b)
# Counting the frequency of each word using "tabulate function"
bword_count<- tabulate(c(index_b))
# Calculating the probabilty of each word in b by calculating 
# frequency_of_each_word/sum_of_all_frequencies
prob_b <- bword_count/sum(bword_count)
# Taking a sample=50 words out of the vector b using probability 
# density in the sample function
b_50 <- sample(b,50,prob=prob_b)
# Using cat function to display the sample as a paragraph
cat(b_50,"\n")

# ------- Ans 10 ------- #
#Navya's part for Practical-1

#Q10:

#let a2 be a vector that holds unique letters of a (to match with b) in lowercase.
a2 <- tolower(unique(a))
#let modified b be b2 and have the stored vector b which was found previously.
b2 <- c(b)
# Find the indices of words in 'b' that are lowercase in 'a2'
indices_to_replace <- which(b %in% a2)
#indexing them:
indexing_2 <- match(b2[indices_to_replace], a2)
# Replace the words in b with corresponding words from a vector b2, indexing:
b2[indices_to_replace] <- a[indexing_2]
# Print the resultant modified b vectore
print(b2)

