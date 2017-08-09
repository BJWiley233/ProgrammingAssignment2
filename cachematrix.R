## need to install and load these packages first
install.packages("MASS")
install.packages("matrixcalc")
library(MASS) 
library(matrixcalc) 

##non-square matrix
NCols <- sample(3:6, 1)
NRows <- sample(2:8, 1)

myMat <- matrix(runif(NCols*NRows), ncol=NCols)
is.square.matrix(myMat)

## functions
makeCacheMatrix <- function(x) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- if (is.square.matrix(x) == TRUE) {
                function(solve) m <<- solve
        }
        else {
                function(ginv) m <<- ginv
        }
        getInv <- function() m
        list(x, set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <-  if (is.square.matrix(data) == TRUE) {
                solve(data, ...)
        }
        else {
                ginv(data, ...)
        }
        x$setInv(m)
        m
}

## run functions for myMat
notSquare <- makeCacheMatrix(myMat) 
cacheSolve(notSquare)


##check
ginv(myMat)