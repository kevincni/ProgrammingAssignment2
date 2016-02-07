## Put comments here that give an overall description of what your
## functions do

# Make object to store a mtrix

makeCacheMatrix <- function(x = matrix()) {
#set matrix to null
    inv <- NULL
#set x to y; reset inv to null
    set <- function(y) {x <<- y; inv <<- NULL}
#return vector x
    get <- function() x
#set inv to inverse
    setInverse <- function(inverse) inv <<- inverse
#return the inverse inv
    getInverse <- function() inv
#return special vector    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) 
}


# Return the inverse matrix of input
# Retrieves cached inverse if input
# is same as last previous. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

#test caching capability
mat1<-matrix(c(seq(1:4),seq(6:10)),nrow=3,ncol=3)
CachedMatrix<-makeCacheMatrix(mat1)
cacheSolve(CachedMatrix)
cacheSolve(CachedMatrix)