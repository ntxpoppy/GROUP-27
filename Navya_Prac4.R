#Q1 solution

#storing number of nodes in every layer in a vector
layer_nodes<- c(3, 4, 4, 2)

#adding an empty list for neural network which will contain 'h' list of nodes 
#'W' a list of matrices and 'b' list of offset parameters
nn <- list()

#Initializing node values 'h' and storing as a list to 'nn' with 
#that vector being the same length as the 'layer_nodes'.
nn$h <- vector("list", length = length(layer_nodes))

#iterating through the elements of 'layer_nodes' 
# only from input layers until the last intermediate layer.
for (l in 1:length(layer_nodes)-1) {
  
  #creating a numeric vector of lth elements with nodes of l layer
  nn$h[[l]] <- numeric(layer_nodes[l])
}

#Initializing weights matrices 'W' and storing as list to 'nn' with 
#length of required interconnecting layers.
nn$W <- vector("list", length = length(layer_nodes)-1)

#Initializing offset parameter matrices 'b' and storing as list to 'nn' with
#length of required interconnecting layers.
nn$b <- vector("list", length = length(layer_nodes)-1)

# iterating through every layer of the network (except the last/output layer)
for (l in 1:(length(layer_nodes)-1)) {
  
  #Producing result weight 'W' matrix under random uniform distribution
  #between the values 0 and 0.2. 
  #Matrix dimensions calculated with total number of nodes in the consequent layer
  #and those in present layer
  nn$W[[l]] <- matrix(runif(layer_nodes[l+1]*layer_nodes[l], 0, 0.2), 
                      nrow = layer_nodes[l+1], ncol = layer_nodes[l])
  

  #Producing matrix 'b' (offset vectors) using random uniform distribution
  #between the values 0 and 0.2. 
  #Matrix dimensions calculated with total number of nodes in the consequent layer.
  nn$b[[l]] <- runif(layer_nodes[l + 1], 0, 0.2)
}

#nodes of first layer stored in vector 'inp'.
nn$h[[1]] <- inp

#iterating through number of nodes except for the last one.' 
for (l in 1:(length(nn$h) - 1)) {
  
  #using ReLu transform
  nn$h[[l + 1]] <- pmax(0, nn$W[[l]] %*% nn$h[[l]] + nn$b[[l]])
}
