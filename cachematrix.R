## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a matrix that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                              
  set <- function(y) {                     
    x <<- y                             
    m <<- NULL                        
  }
  get <- function() x                     
  setinverse <- function(inverse) m <<- inverse  
  getinverse <- function() m                    
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)                                                                                    ## to the functions with the $ operator
}


## Write a short comment describing this function

# cacheSolve returns an inversed matrix made from makeCacheMatrix.
# If the inverse exists in cache, the function retrieve it. 
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
