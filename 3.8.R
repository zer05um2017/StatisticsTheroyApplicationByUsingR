# ex 3-24
x <- c(0,1,2,3)
fx <- c(1/8, 3/8, 3/8, 1/8)
xfx <- x*fx
t <- t(rbind(x,fx,xfx))
rbind(t, apply(t, 2, sum))

# ex 3-25
x <- c(0,1,2,3,4)
fx <- c(0.1,0.2,0.4,0.2,0.1)
xfx <- x*fx
x_mu <- x-sum(xfx)
x_mu2 <- x_mu^2
x_mu2fx <- x_mu2*fx
t <- t(rbind(x, fx, xfx, x_mu, x_mu2, x_mu2fx))
rbind(t, apply(t, 2, sum))

# ex 3.26
x <- c(0,1,2,3,4)
fx <- c(0.1,0.2,0.4,0.2,0.1)
xfx <- x*fx
x2fx <- x^2*fx
t <- t(rbind(x,fx,xfx,x2fx))
rbind(t, apply(t, 2, sum))
sigma2 <- sum(x2fx)-sum(xfx)^2
sigma <- sqrt(sigma2)

