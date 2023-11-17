# ------- Step1 ------- #
# We write a function netup that takes a vector d representing 
# the number of nodes in each layer and returns a list 
# representing the neural network. This list should contain 
# elements for nodes (h), weight matrices (W), and offset vectors 
# (b). We initialize weights and offsets with random deviates.

netup <- function(d) {
  
  # Initialize a list to store neural network components
  nn <- list()
  
  # Initialize node values 'h' as a list with the same length as 'd'
  nn$h <- vector("list", length = length(d))
  
  # Initialize node values for each layer
  for (l in 1:length(d)) {
    nn$h[[l]] <- numeric(d[l])
  }
  
  # Initialize weights matrices 'W' and offset parameter matrices 'b'
  nn$W <- vector("list", length = length(d) - 1)
  nn$b <- vector("list", length = length(d) - 1)
  
  # Iterate through layers to initialize weights and offsets
  for (l in 1:(length(d) - 1)) {
    nn$W[[l]] <- matrix(runif(d[l+1] * d[l], 0, 0.2), 
                        nrow = d[l+1], ncol = d[l])
    nn$b[[l]] <- runif(d[l + 1], 0, 0.2)
  }
  
  # Return the initialized neural network
  return(nn)
}

# ------- Step2 ------- #
# Implement a function forward(nn, inp) that takes a neural 
# network and input values for the first layer (inp). 
# It computes the node values for each layer using the 
# ReLU activation function and returns the updated network.

forward <- function(nn, inp=nn$h[[1]]) {
  
  #iterating through the weights for layers
  for (l in seq_along(nn$W)) {
    
    #forward pass for each layer using the ReLU activation function
    nn$h[[l + 1]] <- pmax(0, nn$W[[l]]%*%nn$h[[l]] + nn$b[[l]])
  }
  #return the updated neural network
  return(nn)
}
