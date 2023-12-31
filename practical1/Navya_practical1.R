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