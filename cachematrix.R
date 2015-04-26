#################################################################
#
# makeCacheMatrix
#
# This is an data function containing two attributes: 
# an invertible matrix and its inverse. This function 
# does not perform the inversion. It is akin to a struct.
# See cacheSolve below.

makeCacheMatrix <- function(mat = matrix()) {
  invMat <- NULL # initialize the inverse
  
  #set and get for matrix
  setMat <- function(newMat) {
    mat <<- newMat; 
    invMat <<- NULL #reinitializes the parent frame invMat to NULL
  }
  getMat <- function(){mat}
  
  # set and get for Inverse matrix
  setInverse <- function(newInverse){
    invMat <<- newInverse  
  }
  getInverse <- function(){invMat}
  
  # creates an list-type API for this function's set-and-get methods
  list(setMat = setMat, 
       getMat = getMat,
       setInverse = setInverse,
       getInverse = getInverse)
}

#############################################################
#
# cacheSolve
#
# This is a principal API for the calculating and/or 
# re-reading the inverse of the matrix object associated
# with an invertible matrix. Given the matrix object 
# created by makeCacheMatrix, it returns the inverse, 
# either through direct calculation (with storage
# in the matrix object) or by retrieving the inverse
# from matrix object if it has already been calculated.

cacheSolve <- function(mat, ...) {
  
  # check to see if the inverse has been calculated
  # by retrieving the inverse from the matrix object
  # and return the inverse if it exists
  invMat <- mat$getInverse()
  if(!is.null(invMat)) {
    message("getting cached inverse")
    return(invMat)
  }
  
  # if the inverse has not been calculated previously, 
  # retrieve the matrix from the matrix object
  matData <- mat$getMat()

  # invert the matrix
  invMat <- solve(matData, ...)

  # save the inverse in the matrix object
  mat$setInverse(invMat)
  
  # return the inverse
  invMat
}

# test spec
#
# > source("cachedMatrixInversion.R")
# > myMatrix <- matrix(c(-4,0,0,2),nrow=2,ncol=2)
# > myMatrix
#      [,1] [,2]
# [1,]   -4    0
# [2,]    0    2
# > cacheMatrix <- makeCacheMatrix(myMatrix)
# > cacheSolve(cacheMatrix)
#       [,1] [,2]
# [1,] -0.25  0.0
# [2,]  0.00  0.5
# > cacheSolve(cacheMatrix)
# getting cached inverse
#       [,1] [,2]
# [1,] -0.25  0.0
# [2,]  0.00  0.5

#### These are our instructions, preserved for accuracy
#### and completeness.

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix <- function(x = matrix()) {
#
#}


## Write a short comment describing this function

#cacheSolve <- function(x, ...) {
#        ## Return a matrix that is the inverse of 'x'
#}
