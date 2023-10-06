split_punct <- function(text) {
  # Use strsplit to split text into words and separate punctuation marks
  result <- unlist(strsplit(text, "([[:alnum:]]+|[[:punct:]])"))
  result <- result[result != ""]
  return(result)
}

# Example usage:
text <- "Hello, world! How are you?"
result <- split_punct(text)
print(result)
