##Generate a function to create a special matrix taht can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
  ## initialize the matrix inverse value to NULL
  matrixinverse <- NULL                     
  set <- function(y) {                      
    x <<- y
    ## change the value of inverse of the matrix incase the matrix was changed.
    matrixinverse <<- NULL              
  }
  ## takes the value of the inverse
  get <- function() x                           
  #calculates the inverse of non-singular matrix via the solve function
  setinverse <- function(solve) matrixinverse <<- solve 
  # takes the inverse     
  getinverse <- function() matrixinverse        
  ## passes the value of the function makeCacheMatrix        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

##Generate a function to calculate the inverse. If inverse has already been computed
##then the cacheSolve should retrieve the inverse from the cash
cacheSolve<- function(x, ...) {                 
  matrixinverse <- x$getinverse()
  #if there is inverse, it gets it.
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  #if the inverse is not there, it is computed and then retrieved.
  data <- x$get()                               
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
