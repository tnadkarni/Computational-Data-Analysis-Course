dat <- read.csv("homework3_data/OPCA.csv", header = FALSE)
tev <- read.csv("homework3_data/True_eigvector.csv", header = FALSE)
dat <- as.matrix(dat)
d = 20
rows = d
n = dim(dat)[1]/rows
w <- as.matrix(rep(sqrt(1/d), d))
dist <- double(n)

ss <- 0.01

norm_vec <- function(x) {
  sqrt(sum(x^2))
}

for (i in c(1:n)) {
  st <- (i-1)*rows + 1
  fin <- rows*i
  A <- dat[c(st:fin),]
  #Uncomment below for part (b)
  ss <- 1/(100+i) 
  new_w <- w + ss*(A %*% w)
  w <- new_w/norm_vec(new_w)
  dist[i] <- norm_vec(w-tev)
}

plot(dist, type = "l", col = "blue", xlab = "Iterations")

