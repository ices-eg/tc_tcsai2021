## Read data, prepare matrices
catch <- as.matrix(read.csv("cod_catch.csv", header=TRUE,
                          check.names=FALSE, row.names=1)) / 1000
nyears <- nrow(catch)
nages <- ncol(catch)

N <- F <- Z <- matrix(NA, nrow=T, ncol=A, dimnames=dimnames(C))

## Assume F in terminal year = 0.1 and M = 0.2
F[nyears,] <- 0.1
M <- 0.2
Z <- F + M

## Calculate N in terminal year
N <- catch * Z / (F * (1-exp(-Z)))

## Calculate N and F up to terminal year,
## assuming F[oldest] = avg(3 preceding ages)
for(y in (nyears-1):1)
{
  for(a in 1:(nages-1))
  {
    N[y, a] <- N[y + 1, a + 1] * exp(M) + catch[y, a] * exp(M/2)
    F[y, a] <- log(N[y, a] / N[y + 1, a + 1]) - M
  }
  F[y, nages] <- mean(F[y, nages-(1:3)])
  Z[y, ] <- F[y,] + M
  N[y, nages] <- catch[y, nages] * Z[y, nages] / (F[y, nages]*(1 - exp(-Z[y, nages])))
}

