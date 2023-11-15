backward <- function(nn, k) {
  n_layers <- length(nn$h)
  n_outputs <- length(nn$h[[n_layers]])
  
  # Initialize lists for derivatives
  nn$dh <- vector("list", length = n_layers)
  nn$dW <- vector("list", length = n_layers - 1)
  nn$db <- vector("list", length = n_layers - 1)
  
  # Compute derivatives for the output layer
  nn$dh[[n_layers]] <- ifelse(seq_len(n_outputs) == k, 
                              exp(nn$h[[n_layers]]) / sum(exp(nn$h[[n_layers]])) - 1,
                              exp(nn$h[[n_layers]]) / sum(exp(nn$h[[n_layers]])))
  
  # Backpropagation to compute derivatives for hidden layers
  for (l in rev(seq_len(n_layers - 1))) {
    nn$dW[[l]] <- nn$h[[l]] %*% t(nn$dh[[l + 1]])
    nn$db[[l]] <- nn$dh[[l + 1]]
    nn$dh[[l]] <- ifelse(nn$h[[l]] > 0, nn$W[[l]] %*% nn$dh[[l+1]], 0)
  }
  
  return(nn)
}


train <- function(nn, inp, k, eta = 0.01, mb = 10, nstep = 10000) {
  n_data <- nrow(inp)
  
  for (step in 1:nstep) {
    # Randomly sample mb indices for mini-batch
    indices <- sample(1:n_data, mb, replace = TRUE)
    
    # Initialize gradients
    grad_W <- lapply(nn$W, function(x) matrix(0, nrow = nrow(x), ncol = ncol(x)))
    grad_b <- lapply(nn$b, function(x) numeric(length(x)))
    
    # Compute average gradients over the mini-batch
    for (i in indices) {
      # Forward pass
      updated_network <- forward(nn, inp[i,])
      
      # Backward pass for the corresponding class k
      updated_network <- backward(updated_network, k[i])
      
      # Accumulate gradients
      for (l in seq_along(nn$W)) {
        grad_W[[l]] <- grad_W[[l]] + updated_network$dW[[l]]
        grad_b[[l]] <- grad_b[[l]] + updated_network$db[[l]]
      }
    }
    
    # Update network parameters using the average gradients
    for (l in seq_along(nn$W)) {
      nn$W[[l]] <- nn$W[[l]] - eta * (grad_W[[l]] / mb)
      nn$b[[l]] <- nn$b[[l]] - eta * (grad_b[[l]] / mb)
    }
  }
  
  return(nn)
}
