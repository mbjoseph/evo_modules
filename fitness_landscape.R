# Fitness landscape module
# Oct 20, 2013 by Max Joseph

# Functions: dbivar (calculates bivariate normal log density with no covariance)
# log.dens (calculates the sum of multiple bivariate normal densities)
dbivar <- function(x, y, mu, sig){
  density.x <- dnorm(x, mu[1], sig, log=T)
  density.y <- dnorm(y, mu[2], sig, log=T)
  return(density.x + density.y)
}

log.dens <- function(state, Mu, Sigma){
  X <- state[[1]]
  Y <- state[[2]]
  # Mu and sigma are 2Xnumber of peak arrays
  n.peaks <- length(Sigma)
  intervals <- length(X)
  z <- array(dim=c(intervals, intervals, n.peaks))
  for (i in 1:n.peaks){
    z[, , i] <- outer(X, Y, dbivar, mu=Mu[, i], sig=Sigma[i])
  }
  sumz <- log(apply(exp(z), c(1, 2), sum))
  return(sumz)
}


# Some variable definitions for the first peak
Mu1 <- c(-2, 2)
sig1 <- 1	
Mu2 <- c(2, -2)
sig2 <- 1
Mu <- cbind(Mu1, Mu2)
Sigma <- c(sig1, sig2)

xmin <- -10
xmax <- 10
ymin <- -10
ymax <- 10

x <- seq(xmin, xmax, length = 50)  # vector series x
y <- seq(ymin, ymax, length = 50)  # vector series y

# scaled axes range from 0 to 1
antilogit <- function(x){exp(x) / (1 + exp(x))}
X <- antilogit(x)
Y <- antilogit(y)

st <- list(x, y) # states
test <- log.dens(st, Mu, Sigma)
Z <- exp(test)

## Visualize the landscape in 3 dimensions
par(mfrow=c(1,1))
persp(X, Y, Z, main = "Bivariate Logit-Normal Distribution",
      col="orchid2", theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
      ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)

# use Metropolis algorithm to simulate random walk within fitness landscape
require(mcmc)
out <- metrop(log.dens, initial=c(0, 0), nbatch=1000, Mu=Mu, Sigma=Sigma, scale=.3)
xseq <- out$batch[, 1]
yseq <- out$batch[, 2]
alx <- antilogit(xseq)
aly <- antilogit(yseq)

# visualize population trajectory
filled.contour(X, Y, Z, xlim=c(0, 1.1), ylim=c(0, 1), frame.plot=F,
               color.palette=topo.colors, key.axes=T)
mar.orig <- par("mar")
w <- (3 + mar.orig[2]) * par("csi") * 2.54
layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
contour(X, Y, Z,
        drawlabels = F,
        axes = F, 
        frame.plot = F,
        add = T)
mtext("High fitness", side=4, line=2, adj=1, cex=1.5)
mtext("Low fitness", side=4, line=2, adj=0, cex=1.5)
lines(alx, aly)

