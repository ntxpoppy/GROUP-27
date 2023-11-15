netup <- function(d) {
  # Check if the input vector has at least two elements (input and output layers)
  if (length(d) < 2) {
    stop("Input vector must have at least two elements (input and output layers).")
  }
  
  # Initialize the network
  network <- list(
    h = vector("list", length = length(d)),  # List to store node values for each layer
    W = vector("list", length = length(d) - 1),  # List to store weight matrices
    b = vector("list", length = length(d) - 1)   # List to store offset vectors
  )
  
  # Initialize node values for each layer
  for (l in 1:length(d)) {
    network$h[[l]] <- numeric(d[l])
  }
  
  # Initialize weight matrices and offset vectors
  for (l in 1:(length(d) - 1)) {
    network$W[[l]] <- matrix(runif(d[l] * d[l + 1], 0, 0.2), nrow = d[l], ncol = d[l + 1])
    network$b[[l]] <- runif(d[l + 1], 0, 0.2)
  }
  
  return(network)
}


forward <- function(nn, inp) {
  # Check if the length of input vector matches the number of input nodes
  if (length(inp) != length(nn$h[[1]])) {
    stop("Input vector length does not match the number of input nodes.")
  }
  
  # Set input values for the first layer
  nn$h[[1]] <- inp
  
  # Perform feedforward to compute node values for each layer
  for (l in 2:length(nn$h)) {
    # Compute weighted sum and apply activation function (assuming sigmoid activation)
    nn$h[[l]] <- sigmoid(nn$h[[l - 1]] %*% nn$W[[l - 1]] + nn$b[[l - 1]])
  }
  
  return(nn)
}

# Sigmoid activation function
sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

# Example usage:
# Assuming you have a network created using the netup function
network <- netup(c(3, 4, 2))

# Provide input values for the first layer
input_values <- c(0.5, 0.3, 0.8)

# Perform forward pass to compute node values for each layer
updated_network <- forward(network, input_values)

# Access updated node values
print(updated_network$h)
