## Question 8: To simulate 50-word sections from the model

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

# Generating a 50-word section as para
para <- simu_fn(b, T, P)

# Printing the generated section with cat
cat(para, "\n")

