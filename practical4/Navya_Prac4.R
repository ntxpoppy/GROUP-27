#Q1 solution

#Defining a function to initialize the neural network taking input d
netup <- function(d) {
  
  #adding an empty list for neural network which will contain 'h' list of nodes 
  #'W' a list of matrices and 'b' list of offset parameters
  nn <- list()
  
  #Initializing node values 'h' and storing as a list to 'nn' with 
  #that vector being the same length as the 'd'.
  nn$h <- vector("list", length = length(d))
  
  #iterating through the elements of 'd' 
  # only from input layers until the last intermediate layer.
  for (l in 1:length(d)) {
    
    #creating a numeric vector of lth elements with nodes of l layer
    nn$h[[l]] <- numeric(d[l])
  }
  
  #Initializing weights matrices 'W' and storing as list to 'nn' with 
  #length of required interconnecting layers.
  nn$W <- vector("list", length = length(d) - 1)
  
  #Initializing offset parameter matrices 'b' and storing as list to 'nn' with
  #length of required interconnecting layers.
  nn$b <- vector("list", length = length(d) - 1)
  
  #iterating through number of nodes except for the last one.
  for (l in 1:(length(d) - 1)) {
    nn$W[[l]] <- matrix(runif(d[l] * d[l + 1], 0, 0.2), nrow = d[l], ncol = d[l + 1])
    nn$b[[l]] <- runif(d[l + 1], 0, 0.2)
  }
  
  #returning the updated list
  return(nn)
}
