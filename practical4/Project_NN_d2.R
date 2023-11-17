#Draft1
rm(list=ls())

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
    nn$W[[l]] <- matrix(runif(d[l+1] * d[l], 0, 0.2), 
                        nrow = d[l+1], ncol = d[l])
    nn$b[[l]] <- runif(d[l + 1], 0, 0.2)
  }
  
  #returning the updated list
  return(nn)
}

# Q2: Define a function to perform the forward pass
forward <- function(nn, inp=nn$h[[1]]) {
  for (l in seq_along(nn$W)) {
    nn$h[[l + 1]] <- pmax(0, nn$W[[l]]%*%nn$h[[l]] + nn$b[[l]])
  }
  return(nn)
}

# Q3 solution

backward <- function(nn, k) {
  n_layers <- length(nn$h)
  n_data <- length(k)
  eta <- 0.01
  
  # Helper function to compute derivatives for the output layer
  compute_output_layer_derivative <- function(h_output, k) {
    exp_h <- exp(h_output)
    softmax_probs <- exp_h / sum(exp_h)
    softmax_probs[k] <- softmax_probs[k] - 1
    return(softmax_probs)
  }
  
  # Helper function to compute derivatives for hidden layers
  #compute_hidden_layer_derivative <- function(W, h, dh_next) {
  # ifelse(h > 0, t(W) %*% dh_next, 0)
  #}
  
  
  backward_output_layer <- function(nn, k, updated_network) {
    n_layers <- length(nn$h)
    
    # Compute derivatives for the output layer
    #nn$dh[[n_layers]] <- compute_output_layer_derivative
    #(updated_network$h[[n_layers]], k)
    
    
    # Backpropagation to compute derivatives for hidden layers
    for (l in rev(seq_len(n_layers - 1))) {
      
      nn$dh[[n_layers]] <- compute_output_layer_derivative(updated_network$
                                                             h[[n_layers]], k)
      
      nn$dW[[l]] <- nn$dh[[l + 1]] %*% t(nn$h[[l]])
      nn$db[[l]] <- nn$dh[[l + 1]]
      
      # Ensure that nn$h[[l]] is a column vector for matrix multiplication
      nn$h[[l]] <- matrix(nn$h[[l]], ncol = 1)
      
      # Update nn$dh[[l]] using matrix multiplication
      nn$dh[[l]] <- ifelse(nn$h[[l]] > 0, t(nn$W[[l]]) %*% nn$dh[[l + 1]], 0)
    }
    
    return(nn)
  }
  
  # Initialize lists for derivatives
  nn$dh <- vector("list", length = n_layers)
  nn$dW <- vector("list", length = n_layers - 1)
  nn$db <- vector("list", length = n_layers - 1)
  
  # Initialize loss and average loss
  total_loss <- 0
  
  # Compute derivatives for the output layer and accumulate loss
  for (i in 1:n_data) {
    # Forward pass
    updated_network <- forward(nn, inp[i, ])
    
    # Compute loss for data point i
    pk <- exp(updated_network$h[[n_layers]]) / sum(exp(updated_network$
                                                         h[[n_layers]]))
    total_loss <- total_loss - log(pk[k[i]])
    
    # Backward pass for the corresponding class k
    nn <- backward_output_layer(updated_network, k[i], nn)
    
    # Accumulate gradients
    for (l in seq_along(nn$W)) {
      nn$dW[[l]] <- nn$dW[[l]] + updated_network$dW[[l]]
      nn$db[[l]] <- nn$db[[l]] + updated_network$db[[l]]
    }
  }
  
  # Calculate average loss
  average_loss <- total_loss / n_data
  
  # Update network parameters using the average gradients
  for (l in seq_along(nn$W)) {
    nn$W[[l]] <- nn$W[[l]] - eta * (nn$dW[[l]] / n_data)
    nn$b[[l]] <- nn$b[[l]] - eta * (nn$db[[l]] / n_data)
  }
  
  return(nn)
}

# Backward pass for the corresponding class k

#Question 4 solution

# Function to perform the training of the neural network using SGD
train <- function(nn, inp, k, eta = 0.01, mb = 10, nstep = 10000) {
  n_data <- nrow(inp)
  
  # Initialize gradients for the mini-batch
  mini_batch_dW <- lapply(nn$W, function(x) matrix(0, nrow = nrow(x), 
                                                   ncol = ncol(x)))
  mini_batch_db <- lapply(nn$b, function(x) numeric(length(x)))
  
  
  for (step in 1:nstep) {
    # Randomly sample mb indices for mini-batch
    indices <- sample(1:n_data, mb, replace = TRUE)
    
    # Compute gradients and loss for the mini-batch
    for (i in indices) {
      # Initialize loss
      total_loss <- 0
      
      # Forward pass
      updated_network <- forward(nn, inp[i,])
      
      # Compute loss for the data point i
      pk <- exp(updated_network$h[[length(updated_network$h)]]) / 
        sum(exp(updated_network$h[[length(updated_network$h)]]))
      total_loss <- total_loss - log(pk[k[i]])
      
      # Backward pass for the corresponding class k
      updated_network <- backward(updated_network, k[i])
      
      # Accumulate gradients
      for (l in seq_along(nn$W)) {
        nn$dW <- lapply(nn$W, function(x) matrix(0, nrow = as.numeric(nrow(x)), 
                                                 ncol = as.numeric(ncol(x))))
        nn$db <- lapply(nn$b, function(x) numeric(length(x)))
        
      }
    }
    
    # Update network parameters using the average gradients
    for (l in seq_along(nn$W)) {
      nn$W[[l]] <- nn$W[[l]] - eta * (nn$dW[[l]] / mb)
      nn$b[[l]] <- nn$b[[l]] - eta * (nn$db[[l]] / mb)
    }
    
    # Print average loss for monitoring training progress
    if (step %% 200 == 0) {
      cat("Step:", step, "   Average Loss:", total_loss / mb, "\n")
    }
  }
  
  return(nn)
}


# Q5 solution
#Load the iris dataset and preprocess it
data(iris)
set.seed(69)  # Set a seed for reproducibility
iris <- iris[sample(nrow(iris)), ]  # Shuffle the dataset

# Split the data into training and test sets
n_train <- floor(0.85 * nrow(iris))
train_data <- iris[1:n_train, -5]
train_labels <- as.integer(iris[1:n_train, 5])
test_data <- iris[(n_train + 1):nrow(iris), -5]
test_labels <- as.integer(iris[(n_train + 1):nrow(iris), 5])

# Train a 4-8-7-3 network
nn <- netup(c(4, 8, 7, 3))
nn <- train(nn, train_data, train_labels)

# Q6: Classify the test data and calculate the misclassification rate
# Function to predict class labels for a given set of inputs
predict_class <- function(nn, inp) {
  # Forward pass
  forward_pass <- forward(nn, inp)
  
  # Find the index of the class with the highest probability
  predicted_class <- which.max(forward_pass$h[[length(nn$h)]])
  
  return(predicted_class)
}

# Function to compute the misclassification rate
compute_misclassification_rate <- function(nn, test_data, test_labels) {
  n_test <- nrow(test_data)
  misclassified <- 0
  
  for (i in 1:n_test) {
    # Predict the class label for each test instance
    predicted_class <- predict_class(nn, test_data[i, ])
    
    # Check if the predicted class is different from the true class
    if (predicted_class != test_labels[i]) {
      misclassified <- misclassified + 1
    }
  }
  
  # Compute misclassification rate
  misclassification_rate <- misclassified / n_test
  
  return(misclassification_rate)
}

# Use the trained neural network to predict class labels for the test set
misclassification_rate <- compute_misclassification_rate(nn, test_data, 
                                                         test_labels)

# Print the misclassification rate
cat("Misclassification Rate:", misclassification_rate, "\n")