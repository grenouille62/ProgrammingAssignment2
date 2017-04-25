## There is 2 functions that implement inverse of a matrix with cache capacity
## 1) makeCacheMatrix : make a list of 4 functions set/get/setResult/getResult.
##    argument : a matrix
##    Return : list of 4 functions set/get/setResult/getResult
## 2) cacheSolve : function that return the inverse of an invertible matrix. 
##    argument : a list made by makeCacheMatrix and an invetible matrix OR just an invertible matrix
##    Return : inverse of the matrix
## 
## Usage example : 
## cma <- makeCacheMatrix(matrix(data=c(1,3,2,4), nrow=2, ncol=2))
## cacheSolve (cma)
## Output :
## Cacheable matrix
##       [,1] [,2]
##  [1,] -2.0  1.0
##  [2,]  1.5 -0.5
##  
##  Recall :
##  cacheSolve (cma)
## Output : 
## Cacheable matrix
## Getting inverse matrix from cache
##       [,1] [,2]
##  [1,] -2.0  1.0
##  [2,]  1.5 -0.5


## Usage : makeCacheMatrix (ma) where ma = an invertible matrix
## Return : list of 4 functions

## Build a list of 4 functions : set/get/setResult/getResult
## set : save the input matrix (ma) into memory 
## get : retrieve de input matrix (ma) from memory
## setResult : put the result (inverse of ma) into memory 
## getResult : retrieve the result from memory 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## save the input matrix into memory
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## retrieve the input matrix fom memory
  get <- function() x
  
  ## save the result into memory
  setResult <- function(result) m <<- result
  ## retrieve the result from memory
  getResult <- function()  m
  
  ## list of 4 functions : set/get/setResult/getResult
  ## these function are callable
  list(set = set, get = get,
       setResult =   setResult,
       getResult = getResult)
}

## Usage : cacheSolve(x) where x can be :
##   the "special matrix" made with makeCacheMatrix
##   OR an "ordinary" invertible matrix
## Return : the matrix inverse of x

## check the class of x : is it a cacheable matrix ?
## yes : lookup the matrix inverse in memory using getResult function
##      if a result is found then return it.
##      if no result found then compute the inverse matrix by using solve function
##      and put the inverse matrix into memory by using putResult function
## no : if x is a matrix the compute its inverse
##      x is not a matrix --> error
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- NULL
  
  ## is x a list made by makeCacheMatrix ?
  if (identical(names(x), c("set", "get", "setResult", "getResult"))) {
    message ("Cacheable matrix")
    
    ## try to retrieve de result from x
    m <- x$getResult()
    ## result found
    if(!is.null(m)) {
      ## return the result with a message
      message("Getting inverse matrix from cache")
      return(m)
    }
    
    ## no result found
    ## retrieve the input matrix from x
    data <- x$get()
    ## call solve
    m <- solve(data)
    ## save the result into memory of x
    x$setResult(m)
    ## return the result
    return(m)
  } else {
    ## is x a matrix ?
    if (class(x) == "matrix") {
      message ("Native matrix")
      ## call solve
      m <- solve(x)
      ## return the result
      return(m)
    } else {
      ## otherwise : error
      message("Error : the argument must be a matrix or a cacheable matrix made with makeCacheMatrix")
    }
  }
}
