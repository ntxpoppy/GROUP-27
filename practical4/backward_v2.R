backward <- function(nn, k, eta = 0.01) {
  n_layers <- length(nn$h)
  n_data <- length(k)
  
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
    #nn$dh[[n_layers]] <- compute_output_layer_derivative(updated_network$h[[n_layers]], k)
    
    
    # Backpropagation to compute derivatives for hidden layers
    for (l in rev(seq_len(n_layers - 1))) {
      
      nn$dh[[n_layers]] <- compute_output_layer_derivative(updated_network$h[[n_layers]], k)
      
      nn$dW[[l]] <- nn$dh[[l + 1]] %*% t(nn$h[[l]])
      nn$db[[l]] <- c(nn$dh[[l + 1]])
      
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
  
  return(list(nn = nn, average_loss = average_loss))
}

# Backward pass for the corresponding class k



