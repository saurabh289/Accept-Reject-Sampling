library("plotrix")
N = 1e4
x1samp <- numeric(length = N)
x2samp <- numeric(length = N)
x3samp <- numeric(length = N)
x4samp <- numeric(length = N)
x5samp <- numeric(length = N)
x6samp <- numeric(length = N)
x1prop <- numeric(length = N)
x2prop <- numeric(length = N)
x3prop <- numeric(length = N)
x4prop <- numeric(length = N)
x5prop <- numeric(length = N)
x6prop <- numeric(length = N)
samp_eval <- numeric(length = N)

h <- 0.1
x1samp[1] <- runif(1,-1,1)
x2samp[1] <- runif(1,-1,1)
x3samp[1] <- runif(1,-1,1)
x4samp[1] <- runif(1,-1,1)
x5samp[1] <- runif(1,-1,1)
x6samp[1] <- runif(1,-1,1)
x1prop[1] <- runif(1,-1,1)
x2prop[1] <- runif(1,-1,1)
x3prop[1] <- runif(1,-1,1)
x4prop[1] <- runif(1,-1,1)
x5prop[1] <- runif(1,-1,1)
x6prop[1] <- runif(1,-1,1)
acc <- numeric(length = N)
acc[1] <- (x1prop[1]^2 + x2prop[1]^2 + x3prop[1]^2 + x4prop[1]^2 + x5prop[1]^2 + x6prop[1]^2) < 1

for (i in 2:N)
{
  x1prop[i] <- x1samp[i-1] + runif(1,-h,h)
  x2prop[i] <- x2samp[i-1] + runif(1,-h,h)
  x3prop[i] <- x3samp[i-1] + runif(1,-h,h)
  x4prop[i] <- x4samp[i-1] + runif(1,-h,h)
  x5prop[i] <- x5samp[i-1] + runif(1,-h,h)
  x6prop[i] <- x6samp[i-1] + runif(1,-h,h)
  acc[i] <- (x1prop[i]^2 + x2prop[i]^2 + x3prop[i]^2 + x4prop[i]^2 + x5prop[i]^2 + x6prop[i]^2) <1
  if(acc[i]==0)
  {
    x1samp[i] <- xsamp[i-1]
    x2samp[i] <- xsamp[i-1]
    x3samp[i] <- xsamp[i-1]
    x4samp[i] <- xsamp[i-1]
    x5samp[i] <- xsamp[i-1]
    x6samp[i] <- xsamp[i-1]
  }
  else
  {
    x1samp[i] <- x1prop[i]
    x2samp[i] <- x2prop[i]
    x3samp[i] <- x3prop[i]
    x4samp[i] <- x4prop[i]
    x5samp[i] <- x5prop[i]
    x6samp[i] <- x6prop[i]
  }
}
#ind <- xsamp>0

#samp_eval <- xsamp^2 + ysamp^2 - 1


