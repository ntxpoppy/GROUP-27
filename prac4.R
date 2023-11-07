rm(list=ls())
d <- c(3, 4, 4, 2)
nn <- list()

# Initialize node values
nn$h <- vector("list", length = length(d))
for (l in 1:length(d)-1) {
  nn$h[[l]] <- numeric(d[l])
}

# Initialize weight matrices and offset vectors with random values
nn$W <- vector("list", length = length(d)-1)
nn$b <- vector("list", length = length(d)-1)
for (l in 1:(length(d)-1)) {
  nn$W[[l]] <- matrix(runif(d[l+1]*d[l], 0, 0.2), nrow = d[l+1], ncol = d[l])
  nn$b[[l]] <- runif(d[l + 1], 0, 0.2)
}

nn$h[[1]] <- inp
for (l in 1:(length(nn$h) - 1)) {
  nn$h[[l + 1]] <- pmax(0, nn$h[[l]] %*% nn$W[[l]] + nn$b[[l]])
}