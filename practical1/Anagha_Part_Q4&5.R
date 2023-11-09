# Questions 4 and 5: Creating split_punct() AND Separating punctuation marks and words

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