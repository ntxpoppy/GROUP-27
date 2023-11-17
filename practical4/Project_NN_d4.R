#--------------------#
#    Practical 4     #
#--------------------#

# ------- Group Introduction ------- #

# The following members form Group 27:
#   1. Anagha - S2596158
#   2. Anjali - S2580177
#   3. Navya  - S2602687

# Contribution of each member:
# We collectively decided the distribution of questions.
#   1. Anagha - 34% - Q3 and Q4
#   2. Anjali - 33% - Q5 and Q6
#   3. Navya  - 33% - Q1 and Q2

# ------- Overview of the project ------- # 

# The project involves implementing a simple fully connected 
# neural network for a classification task using the ReLU 
# activation function and stochastic gradient descent (SGD) 
# for optimization. The steps required are:
# Step1: Network Initialization
# Step2: Forward Pass
# Step3: Backward Pass
# Step4: Training Function
# Step5: Data Preprocessing and Training
# Step6: Evaluation


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
  for (l in seq_along(nn$W)) {
    # Compute the forward pass using ReLU activation function
    nn$h[[l + 1]] <- ((nn$W[[l]] %*% nn$h[[l]]) + nn$b[[l]])
  }
  return(nn)
}

# ------- Step3 ------- #
# Here we write a function backward(nn, k) to compute the derivatives 
# of the loss corresponding to the output class k. This involves 
# back-propagation to compute derivatives for nodes, weights, and 
# offsets. Update the network with lists dh, dW, and db.

backward <- function(nn, k) {
  #get the number of layers
  n_layers <- length(nn$h)
  #get the number of data points of output class k
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
  backward_output_layer <- function(nn, k, updated_network) {
    n_layers <- length(nn$h)
    
    # Compute derivatives for the output layer
    nn$dh[[n_layers]] <- compute_output_layer_derivative(updated_network$h[[n_layers]], k)
    
    # Backpropagation to compute derivatives for hidden layers
    for (l in rev(seq_len(n_layers - 1))) {
      nn$dh[[l]] <- compute_output_layer_derivative(updated_network$h[[l]], k)
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
    pk <- exp(updated_network$h[[n_layers]]) / sum(exp(updated_network$h[[n_layers]]))
    total_loss <- total_loss - log(pk[k[i]])
    
    # Backward pass for the corresponding class k
    nn <- backward_output_layer(nn, k[i], updated_network)
    
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


# ------- Step4 ------- #
# Implement a training function train(nn, inp, k, eta, mb, nstep) 
# to train the neural network using stochastic gradient descent. 
# It should take input data (inp), labels (k), learning rate (eta), 
# mini-batch size (mb), and the number of optimization steps (nstep).

train <- function(nn, inp, k, eta = 0.01, mb = 10, nstep = 10000) {
  n_data <- nrow(inp)
  
  # Helper function to compute derivatives for hidden layers
  backward_output_layer <- function(nn, k, updated_network) {
    n_layers <- length(nn$h)
    
    # Compute derivatives for the output layer
    nn$dh[[n_layers]] <- compute_output_layer_derivative(updated_network$h[[n_layers]], k)
    
    # Backpropagation to compute derivatives for hidden layers
    for (l in rev(seq_len(n_layers - 1))) {
      nn$dh[[l]] <- compute_output_layer_derivative(updated_network$h[[l]], k)
      nn$dW[[l]] <- nn$dh[[l + 1]] %*% t(nn$h[[l]])
      nn$db[[l]] <- nn$dh[[l + 1]]
      
      # Ensure that nn$h[[l]] is a column vector for matrix multiplication
      nn$h[[l]] <- matrix(nn$h[[l]], ncol = 1)
      
      # Update nn$dh[[l]] using matrix multiplication
      nn$dh[[l]] <- ifelse(nn$h[[l]] > 0, t(nn$W[[l]]) %*% nn$dh[[l + 1]], 0)
    }
    
    return(nn)
  }
  
  # Helper function to compute derivatives for the output layer
  compute_output_layer_derivative <- function(h_output, k) {
    exp_h <- exp(h_output)
    softmax_probs <- exp_h / sum(exp_h)
    softmax_probs[k] <- softmax_probs[k] - 1
    return(softmax_probs)
  }
  
  # Initialize gradients for the mini-batch
  mini_batch_dW <- lapply(nn$W, function(x) matrix(0, nrow = nrow(x), ncol = ncol(x)))
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
      nn <- backward_output_layer(nn, k[i], updated_network)
      
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


# ------- Step5 ------- #
# We load the iris dataset, shuffle it, and split it into training 
# and test sets. Train a neural network with layer dimensions 
# (4-8-7-3).

# loading the iris data from base R
data(iris)
# Setting a seed for reproducibility
set.seed(69)
# Shuffling the dataset using sample function
iris <- iris[sample(nrow(iris)), ]

# Splitting the data into training and test sets

# The number of samples for the training data are 85% of the 
# total dataset, for which we use the floor function.
n_train <- floor(0.85 * nrow(iris))

# Extracting training data and labels based on the logic that
# test data consists of every 5th row and training data becomes
# the remaining data.

# Excluding the 5th column, which contains the class labels
train_data <- iris[1:n_train, -5] 
# Extracting the class labels as integers
train_labels <- as.integer(iris[1:n_train, 5])
# Extracting test data and labels
test_data <- iris[(n_train + 1):nrow(iris), -5]
test_labels <- as.integer(iris[(n_train + 1):nrow(iris), 5])

# Training a 4-8-7-3 network using the netup and train function
nn <- netup(c(4, 8, 7, 3))
nn <- train(nn, train_data, train_labels)


# ------- Step6 ------- #
# Writing a function to predict class labels for the test set 
# using the trained neural network. Computing the misclassification 
# rate for the test set.

# Generating a function to classify the test data
# into the predicted class.
predict_class <- function(nn, inp) {
  # Forward pass for neural network
  forward_pass <- forward(nn, inp)
  
  # Find the index of the class with the highest probability
  predicted_class <- which.max(forward_pass$h[[length(nn$h)]])
  
  return(predicted_class)
}

# Function to compute the misclassification rate, which is the
# proportion of misclassificed samples in the dataset.

compute_misclassification_rate <- function(nn, test_data, test_labels) {
  
  n_test <- nrow(test_data)
  # first we set misclassified rate as 0
  misclassified <- 0
  
  for (i in 1:n_test) {
    # Predict the class label for each test instance
    # using the predict_class function defined above
    predicted_class <- predict_class(nn, test_data[i, ])
    
    # Check if the predicted class is different from the 
    # true class by comparing
    if (predicted_class != test_labels[i]) {
      misclassified <- misclassified + 1
    }
  }
  
  # Computing misclassification rate
  misclassification_rate <- misclassified / n_test
  
  return(misclassification_rate)
}

# Using the trained neural network to predict class labels for the test set
misclassification_rate <- compute_misclassification_rate(nn, test_data, test_labels)

# Print the misclassification rate
cat("Misclassification Rate:", misclassification_rate, "\n")


# ------- Conclusion of the project ------- # 

# In conclusion, the provided code implements a simple fully 
# connected neural network for a classification task using the 
# ReLU activation function and stochastic gradient descent (SGD) 
# for optimization. The steps involve initializing the neural 
# network, performing forward and backward passes, training the 
# network using SGD, and evaluating its performance on a test set.

# Thank you for going through our project. 

#################
#   THANK YOU   #
#################
