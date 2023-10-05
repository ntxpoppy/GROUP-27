## Question 8: To simulate 50-word sections from the model

#Function to generate a 50-word section
word_simulation <- function(common_words, T, P) {
  
  #Initializing an empty character vector
  fifty_word_text <- character(0)  
  
  #Starting with two initial words using sample()
  word1 <- sample(common_words, 1)
  word2 <- sample(common_words, 1)
  
  #Creating the loop for generating 50 words
  for (i in 1:50) {
    # Finding rows in T where word1 is in the first column and word2 is in the second column
    #using which() function by checking their indices
    find_rows <- which(T[, 1] == word1 & T[, 2] == word2)
    
    if (length(find_rows) > 0) {
      # Randomly selecting one of the matching rows
      select_row <- sample(find_rows, 1)
      
      #Getting the next word from the third column of the selected row
      next_word <- common_words[T[select_row, 3]]
    } else {
      # If no matching rows in T, checking P
      match_in_p <- which(P[, 1] == word1)
      if (length(match_in_p) > 0) {
        # Randomly selecting a row from P and getting the second word as the next word
        select_row <- sample(match_in_p, 0.9)
        next_word <- common_words[P[select_row, 2]]
      } else {
        # If no matching pairs in P, choose a common word randomly
        next_word <- sample(common_words, 1)
      }
    }
    
    # Adding the next word to the total text
    fifty_word_text <- c(fifty_word_text, next_word)
    
    # Updating word1 and word2 for the next iteration
    word1 <- word2
    word2 <- next_word
  }
  
  # Returning the generated text as a single string
  return(paste(fifty_word_text, collapse = " "))
}

# Generating a 50-word section
generated_section <- word_simulation(b, T, P)

# Printing the generated section
cat(generated_section, "\n")
