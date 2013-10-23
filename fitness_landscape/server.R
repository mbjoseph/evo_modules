# define operations for server
require(shiny)
require(mcmc)

# define helper functions
#------------------------
antilogit <- function(x){exp(x) / (1 + exp(x))} # scale from 0 to 1

# dbivar (calculates bivariate normal log density with no covariance)
dbivar <- function(x, y, mu, sig){
  density.x <- dnorm(x, mu[1], sig, log=T)
  density.y <- dnorm(y, mu[2], sig, log=T)
  return(density.x + density.y)
}

# log.dens (calculates the sum of multiple bivariate normal densities)
log.dens <- function(state, Mu, Sigma, pow=1){
  X <- state[[1]]
  Y <- state[[2]]
  # Mu and sigma are 2Xnumber of peak arrays
  n.peaks <- length(Sigma)
  intervals <- length(X)
  z <- array(dim=c(intervals, intervals, n.peaks))
  for (i in 1:n.peaks){
    z[, , i] <- outer(X, Y, dbivar, mu=Mu[, i], sig=Sigma[i])
  }
  sumz <- log(apply(exp(z)^pow, c(1, 2), sum))
  return(sumz)
}

# define_landscape (generates peak positions & standard deviations)
define_landscape <- function(random = FALSE, n.peaks = NULL, 
                             pow=1, fineness=150){
  ## Define some parameters ##
  fineness <- 150
  x <- seq(-10, 10, length = fineness)  # vector series x
  y <- seq(-10, 10, length = fineness)  # vector series y
  X <- antilogit(x)
  Y <- antilogit(y)
  st <- list(x, y) # initialize states
  if (random == FALSE){ # default peaks
    Mu1 <- c(-2, 2)
    sig1 <- 1  
    Mu2 <- c(2, -2)
    sig2 <- 1
    Mu <- cbind(Mu1, Mu2)
    Sigma <- c(sig1, sig2)
  } else { # random peaks
    if (!is.numeric(n.peaks)){
      n.peaks <- runif(1, 10, 30)
    }
    Mu <- array(runif(2*n.peaks, -5, 5),
                dim=c(2, n.peaks))
    Sigma <- runif(n.peaks, 1, 2)
  }
  # calculate density
  test <- log.dens(st, Mu, Sigma, pow=pow)
  Z <- exp(test)
  return(list(Mu=Mu, Sigma=Sigma, X=X, Y=Y, Z=Z))
}

sims <- function(strength, rand, ngen, mutation){
  landscape <- define_landscape(random = rand, pow=strength)
  
  # use Metropolis algorithm to simulate random walk within fitness landscape
  out <- metrop(log.dens, initial=c(0, 0), nbatch=(100 + ngen), 
                Mu=landscape$Mu, Sigma=landscape$Sigma, 
                scale=mutation, pow=strength)
  xseq <- out$batch[-(1:100), 1]
  yseq <- out$batch[-(1:100), 2]
  alx <- antilogit(xseq)
  aly <- antilogit(yseq)
  return(list(X=landscape$X, Y=landscape$Y, Z=landscape$Z, alx=alx, aly=aly))
}

# end helper functions

# shiny code
#-----------
shinyServer(function(input, output){
  widgets <- reactive({
    list(strength=input$strength, 
         ngen=input$ngen, 
         mutation=input$mutation, 
         rand=input$random)
  })
  
  output$p1 <- renderPlot(function(){
    res <- with(widgets(), {
      sims(strength, rand, ngen, mutation)
    })

    with(res, {
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
    })
  })
})