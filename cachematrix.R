## When deailing with very large files inversion of a Matrix can be a very expensive operation
## from a computation perspective.  Caching the inverse of a Matric can help with this repeatedly
## costly operation. MakeCacheMatrix and cacheSolve cache the inverse of a matrix. 

## the makeCacheMatrix will create a list with functions to set the value of a matrix, then 
## get the value of the same matrix, followed by setting the inverste of the same matrix, where
## finally it gets the valyue of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## set value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x   ## get value of matrix
  setinverse <- function(inverse) inv <<- inverse       ## set inverse of matrix
  getinverse <- function() inv ## get values of matrix inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## the cacheSolve function returns the inverse of the matrix while first checking to
## if the inverse has already been computed.  If already computed, cacheSolve skips
## the unnecessary computation while getting the already existing inverse
## from cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {   ## checking for already existing cached version if it exists.
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## A sample run can be created via the R Command line with the following syntaxes
## x = rbind(c(9, -3), c(-3, 9))
## y = makeCacheMatrix(x)
## y$get() will allow you to view the defined matrix values.
##     [,1] [,2]
## [1,]    9   -3
## [2,]   -3    9
## Run 1 will solve the inverse, load into cache, then display values
## cacheSolve(y)
##
##            [,1]       [,2]
## [1,] 0.12500000 0.04166667
## [2,] 0.04166667 0.12500000
## Run 2 will get the already committed to cache values with a message as proof
## cacheSolve(y)
## getting cached data.
##          [,1]       [,2]
##[1,] 0.12500000 0.04166667
##[2,] 0.04166667 0.12500000
