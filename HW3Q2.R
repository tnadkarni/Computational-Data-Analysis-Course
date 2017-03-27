dataset <- read.csv("homework3_data/MLR.csv", header = FALSE)
X <- data.matrix(dataset[,c(1:30)])
X <- cbind(1,X)
y <- data.matrix(dataset[,31])
beta <- data.matrix(rep(0.5, dim(X)[2]))
beta_new <- data.matrix(rep(0, dim(X)[2]))
b <- 1 # Batch : b=10,25,100

# f(Beta)
cost <- function(X, y, beta) {
  sum( (X %*% beta - y)^2 ) / (2*length(y))
}

norm_vec <- function(x) {
  sum(x^2)
}

num_iters <- 1000 
tolerance <- 1e-6 #0.0005 for batch

getGrad <- function(rows, beta) {
  gradients <- sapply(rows, function(i) {
    rowx <- t(as.matrix(X[i,]))
    error <- rowx %*% beta - y[i,]
    grad <- t(rowx) %*% error
    return(grad)
  })
  return(rowMeans(gradients))
}
#stepsize
eig <- eigen((1/dim(y)[1])*t(X)%*%X)
L <- max(eig$values) 
alpha <- b/(length(y)*L)

# keep history
cost_history <- double(num_iters)

i <- 1
grad <- 1
row <- sample(1:1000,b, replace = T) 

#gradient descent
while (norm_vec(beta_new-beta) > tolerance) {
  print(norm_vec(beta_new-beta))
  beta <- beta_new
  rows <- sample(1:1000,b) 
  grad <- as.matrix(getGrad(rows, beta))
  beta_new <- beta - alpha*grad
  cost_history[i] <- cost(X, y, beta_new)
  i <- i+1
}

cost_history[i:num_iters] <- cost_history[i-1]

#plot of f(beta)
plot(cost_history[1:i], type = "l", col = "red",xlab = "Iterations", 
     ylab = "f(Beta)")

trueBeta <- read.csv("homework3_data/True_Beta.csv", header = FALSE)


MSE <- norm_vec(beta[2:31] - trueBeta)/30
MSE

