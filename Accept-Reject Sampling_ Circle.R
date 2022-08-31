library("plotrix")
N = 1e4
xsamp <- numeric(length = N)
ysamp <- numeric(length = N)
xprop <- numeric(length = N)
yprop <- numeric(length = N)
acc <- numeric(length = N)
acc[1] <- 1
samp_eval <- numeric(length = N)

xsamp[1] <- .2
ysamp[1] <- .6
xprop[1] <- .2
yprop[1] <- .6

for (i in 2:N)
{
  xprop[i] <- runif(1,-1,1)
  yprop[i] <- runif(1,-1,1)
  acc[i] <- (xprop[i]^2 + yprop[i]^2) <1
  if(acc[i]==0)
  {
    xsamp[i] <- xsamp[i-1]
    ysamp[i] <- ysamp[i-1]
  }
  else
  {
    xsamp[i] <- xprop[i]
    ysamp[i] <- yprop[i]
  }
}

samp_eval <- xsamp^2 + ysamp^2 - 1
colors <- ifelse(acc,"blue","red")

x <- seq(-1.1,1.1,length = 1e4)
y <- seq(-1.1,1.1,length = 1e4)

for(i in 1:1e4)
{
  plot(x,y,type="n",col="red", main= "Accept Reject Sampling from a circle", ylim = range(y), xlim = range(x))
  draw.circle(0,0,radius = 1)
  points(xsamp[i],ysamp[i], col="green", pch=16, cex =1)
  points(xsamp[1:i-1],ysamp[1:i-1], pch=16, cex=1)
  points(xprop[i+1],yprop[i+1],col = colors[i+1],pch=16, cex =1 )
  Sys.sleep(0.05)
}
plot(x,y,type="n",col="red",main="Accept-Reject Sampling in Circle")
draw.circle(0,0,radius = 1)
points(xprop,yprop,col = colors, pch= 16, cex= 1)

par(mfrow=c(1,2))
acf(xsamp, main=paste("Autocorrelation for x for h = "),h)


