ReturnCoins <- function(M)
{
  # This function returns the minimum number of coins whose total value is equal to M
  r <- M
  pd <- floor(r/50)
  r <- r - 50*pd
  dc <- floor(r/20)
  r <- r - 20*dc
  ds <- floor(r/10)
  r <- r - 10*ds
  p <- floor(r/5)
  r <- r-5*p
  d <- floor(r/2)
  j <- r-2*d
  
  return(c(pd, dc, ds, p, d, j))
  
    
} 
res <- ReturnCoins(120)
res

################################################################################
#PSEUDOCODE
# UniversalReturnCoins(M,c)
# r <- M
# d <- number of denominations in c
# for k <- 1 to d
# i[k] <- r/c[k]
# r <- r - c[k]*i[k]
# return i 


UniversalReturnCoins <- function(M,c)
{ # This function returns the minimum number of coins whose total value is equal to M
  # Any currency
  
  r <- M
  d <- length(c)
  i <- c()
  for (k in 1:d)
  {
    i[k] <- floor(r/c[k])
    r <- r - c[k]*i[k]
  }
  return (i) 
}

# definition of currences
ceske <- c(50, 20, 10, 5, 2, 1)
eura <- c(2, 1, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01)

res <- UniversalReturnCoins(12.58, eura)
res

###############################################################################
#RECURSION - the most chocolate path
#PSEUDOCODE
# Chocolate(M,r, c)
# if r = numer or rows in M  #  base case
#   return M(r,c)
# else
#   bars <- M(r,c)  # save value of chocolate we have
#   down <- Chocolate(M, r+1, c) # calling of the function
#   diagonal <- Chocolate(M, r+1, c+1)
#   return max(down, diagonal) + bars


Chocolate <- function(M,r,c)
{
  if (r == nrow(M))
  {
    return(M[r,c])
  }
  else
 {
    bars <- M[r,c]  # save value of chocolate we have
    down <- Chocolate(M, r+1, c) # calling of the function
    diagonal <- Chocolate(M, r+1, c+1)
 
  return(max(down, diagonal) + bars)
  }

}

mat = matrix(c(8,4,5,6,7,9,2,3,4),nrow = 3, ncol = 3,byrow = TRUE)
mat
bars <- Chocolate(mat, 1,1)
bars


###############################################################################
#RECURSION - the tower of hanoi
#PSEUDOCODE
# HanoiTowers(n, fromPeg, toPeg)
# if n = 1
#   output 'Move disc from fromPeg to toPeg'
#   return
# unusedPeg <- 6-fromPeg-toPeg
# HanoiTowers (n-1, fromPeg, emptyPeg)
# output 'Move disc from fromPeg to toPeg'
# HanoiTowers (n-1, fromPeg, toPeg)



HanoiTowers <- function(n, fromPeg, toPeg) {
  if (n == 1) {
    cat("Move disc from peg", fromPeg, "to peg", toPeg, "\n")
  } else {
    unusedPeg <- 6 - fromPeg - toPeg # 1+2+3= 6
    HanoiTowers(n - 1, fromPeg, unusedPeg)
    cat("Move disc from peg", fromPeg, "to peg", toPeg, "\n")
    HanoiTowers(n - 1, unusedPeg, toPeg)
  }
}

n <- 3 
fromPeg <- 1
toPeg <- 3
HanoiTowers(n, fromPeg, toPeg)
