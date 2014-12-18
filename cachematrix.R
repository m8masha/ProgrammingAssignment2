## Caching the Data of Inversed Matrix

## 1.1 makeCacheMatrix - this function creates a special object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL # s will be my "solved" matrix
    set <- function(y){ # takes an input matrix
      x <<- y # saves the input matrix
      s <<- NULL # reserts the s to NULL (when a new object is generated)
    }
   
    #next three functions will be used by cacheSolve to get data for s:
     
    get <- function() {x} # returns the value of original matrix
    setSolvedMatrix <- function(solve) {s <<- solve} # will be called by cacheSolve() during the first access
                                                     # and will store the value using super-assignment (i.e. "<<-")
    getSolvedMatrix <- function() {s} # this will return the cached value to cacheSolve on subsequent accesses
    list(set = set, get = get, setSolvedMatrix = setSolvedMatrix, getSolvedMatrix = getSolvedMatrix) # a list of internal
                                                                                                     # functions ('methods')
}

    
## 1.2 cacheSolve - this function computes the inverse of the special "matrix" returned by makeCacheMatrix above, if the 
## inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache.
cacheSolve <- function(x, ...) { # the input x is an object created by makeVector
        s <- x$getSolvedMatrix() # accesses the object 'x' and gets the value of the mean
        if(!is.null(s)) { #'if s isn't NULL, then...'
          
          message("getting inversed matrix")
          return(s) # returns the inversed matrix (that has been 'solved' previously)
        }
        data <- x$get() # we reach this code if s=NULL. We get the data of original matrix
        s <- solve(data, ...) # 'solves' (i.e. inverses) the original matrix
        x$setSolvedMatrix(s) # stores the inversed matrix
        s # returns the inversed matrix to the code that called this function
}
