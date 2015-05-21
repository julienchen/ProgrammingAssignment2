#######################################################################################
## Assignment 2: Caching the Inverse of a Matrix
## #######################################################################################

## makeCacheMatrix： This function creates a special "matrix" object that can cache its inverse
## Attention:  the matrix supplied (x) is always invertible in this assignment.

makeCacheMatrix <- function(x = matrix()) {
  #initialization inverse Matrix
  inverseM <- NULL
  
  # in fact, in this assignment, this set function is usless (unnecessary).
  set <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  
  # get original Matrix
  get <- function() x
  
  # set value of inverse Matrix
  setinverse <- function(inverse) inverseM <<- inverse
  
  # get value of inverse Matrix
  getinverse <- function() inverseM
  
  # return a list of functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## Attention: argument x should be a makeCacheMatrix object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  
  # get the value of inverse Matrix of the input makeCacheMatrix object.
  inverseM <- x$getinverse()
  
  # if inverse Matrix of this object is already calculated, then print information message 
  # and return the value cached in this object.
  if( !is.null(inverseM) ) {
    message("getting cached matrix")
    return(inverseM)
  }
  
  # if this is a new created makeCacheMatrix object (inverseM is NULL)
  # get the orignial input matrix
  orgmatrix <-x$get()
  # calculate the inverse Matrix
  inverseM <- solve(orgmatrix, ...)
  # cache the value of inverse Matrix to this makeCachedMatrix object by setinverse() function.
  x$setinverse(inverseM)
  # return the inverse Matrix without any message.  
  inverseM
  
}

#######################################################################################
# In order to test these two functions, you can simply press the following commands:
#######################################################################################
# > matrice = matrix(1:4,nrow=2,ncol=2)
# > cachedMatrice=makeCacheMatrix(matrice)
# > cacheSolve(cachedMatrice)   
#
# (computes and return the inverse of matrix "matrice")
#
# > cacheSolve(cachedMatrice)
#
# (Just retrieve the inverse of matrix "matrice" from the cache.)
#