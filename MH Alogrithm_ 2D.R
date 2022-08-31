library("plotrix")
N = 1e4
xsamp <- numeric(length = N)
ysamp <- numeric(length = N)
xprop <- numeric(length = N)
yprop <- numeric(length = N)
acc <- numeric(length = N)
acc[1] <- 1
samp_eval <- numeric(length = N)

h <- 0.2
xsamp[1] <- .2
ysamp[1] <- .6
xprop[1] <- .2
yprop[1] <- .6

for (i in 2:N)
{
  propx <- xsamp[i-1] + runif(1,-h,h)
  propy <- ysamp[i-1] + runif(1,-h,h)
  acc[i] <- (propx^2 + propy^2) <1
  if(acc[i]==0)
  {
    xsamp[i] <- xsamp[i-1]
    ysamp[i] <- ysamp[i-1]
  }
  else
  {
    xsamp[i] <- propx
    ysamp[i] <- propy
  }
  xprop[i] <- propx
  yprop[i] <- propy
}
ind <- xsamp>0

samp_eval <- xsamp^2 + ysamp^2 - 1
colors <- ifelse(acc,"blue","red")

x <- seq(-1.1,1.1,length = 1e4)
y <- seq(-1.1,1.1,length = 1e4)

for(i in 1:100)
{
  plot(x,y,type="n",col="red", main= "MCMC Sampling from a circle", ylim = range(y), xlim = range(x))
  draw.circle(0,0,radius = 1)
  points(xsamp[i],ysamp[i], col="green", pch=16, cex =1)
  segments(x0 = xsamp[i]-h,y0= ysamp[i]-h, x1 = xsamp[i]+h, y1 = ysamp[i]-h, lwd = 2)
  segments(x0 = xsamp[i]-h,y0= ysamp[i]-h, x1 = xsamp[i]-h, y1 = ysamp[i]+h, lwd = 2)
  segments(x0 = xsamp[i]-h,y0= ysamp[i]+h, x1 = xsamp[i]+h, y1 = ysamp[i]+h, lwd = 2)
  segments(x0 = xsamp[i]+h,y0= ysamp[i]+h, x1 = xsamp[i]+h, y1 = ysamp[i]-h, lwd = 2)
  points(xsamp[1:i-1],ysamp[1:i-1], pch=16, cex=1)
  points(xprop[i+1],yprop[i+1],col = colors[i+1],pch=16, cex =1 )
  Sys.sleep(0.01)
}

