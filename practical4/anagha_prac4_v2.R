backward <- function(nn, k) {
  n_layers <- length(nn$h)
  n_data <- length(k)
  
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
    nn <- backward_output_layer(updated_network, k[i])
    
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
  
  return(list(nn = nn, average_loss = average_loss))
}

# Helper function to compute derivatives for the output layer
backward_output_layer <- function(nn, k) {
  n_layers <- length(nn$h)
  
  # Compute derivatives for the output layer
  nn$dh[[n_layers]] <- compute_output_layer_derivative(nn$h[[n_layers]], k)
  
  # Backpropagation to compute derivatives for hidden layers
  for (l in rev(seq_len(n_layers - 1))) {
    nn$dW[[l]] <- outer(nn$dh[[l + 1]], nn$h[[l]])
    nn$db[[l]] <- nn$dh[[l + 1]]
    nn$dh[[l]] <- compute_hidden_layer_derivative(nn$W[[l]], nn$h[[l]], nn$dh[[l + 1]])
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

# Helper function to compute derivatives for hidden layers
compute_hidden_layer_derivative <- function(W, h, dh_next) {
  return(ifelse(h > 0, W %*% dh_next, 0))
}



# Function to perform the training of the neural network using SGD
train <- function(nn, inp, k, eta = 0.01, mb = 10, nstep = 10000) {
  n_data <- nrow(inp)
  
  for (step in 1:nstep) {
    # Randomly sample mb indices for mini-batch
    indices <- sample(1:n_data, mb, replace = TRUE)
    
    # Initialize gradients
    nn$dW <- lapply(nn$W, function(x) matrix(0, nrow = nrow(x), ncol = ncol(x)))
    nn$db <- lapply(nn$b, function(x) numeric(length(x)))
    
    # Initialize loss
    total_loss <- 0
    
    # Compute gradients and loss for the mini-batch
    for (i in indices) {
      # Forward pass
      updated_network <- forward(nn, inp[i,])
      
      # Compute loss for the data point i
      pk <- exp(updated_network$h[[length(updated_network$h)]]) / sum(exp(updated_network$h[[length(updated_network$h)]]))
      total_loss <- total_loss - log(pk[k[i]])
      
      # Backward pass for the corresponding class k
      updated_network <- backward(updated_network, k[i])
      
      # Accumulate gradients
      for (l in seq_along(nn$W)) {
        nn$dW[[l]] <- nn$dW[[l]] + updated_network$dW[[l]]
        nn$db[[l]] <- nn$db[[l]] + updated_network$db[[l]]
      }
    }
    
    # Update network parameters using the average gradients
    for (l in seq_along(nn$W)) {
      nn$W[[l]] <- nn$W[[l]] - eta * (nn$dW[[l]] / mb)
      nn$b[[l]] <- nn$b[[l]] - eta * (nn$db[[l]] / mb)
    }
    
    # Print average loss for monitoring training progress
    if (step %% 100 == 0) {
      cat("Step:", step, "   Average Loss:", total_loss / mb, "\n")
    }
  }
  
  return(nn)
}

# Example usage:
# trained_network <- train(initialized_network, input_data, true_labels, eta = 0.01, mb = 10, nstep = 10000)
