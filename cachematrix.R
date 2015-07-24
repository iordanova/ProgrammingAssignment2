##EXAMPLE USE
## source("cachematrix.R")
## z<-matrix(c(23,4,5,6),2,2)
## d<-makeCacheMatrix(z)
## cacheSolve(d)

## Functional extension to "matrix"
## - provides access function to the input matrix
## - provides access function to its inverse
##    - Note: does NOT calculate inverse,
##    - initializes it to default (NULL)

makeCacheMatrix <- function(x = matrix()) {
      ## init inv to NULL
      inv<-NULL
      
      ##function set: sets the matrix to the input one
      set<-function(y){
        x<<-y ##set the x to y
        inv<<-NULL ##reset the inverse to NULL  
      }
      
      ##function get: returnes the input matrix
      get <- function() x
      ##function setinverse: sets inv to the inverse matrix
      setinverse <- function(inverse) inv <<- inverse 
      ##function getinverse: gets the inverse function
      getinverse <- function() inv
  
      #return list of makeCacheMatrix
      list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Return inverse of a  matrix 'x'
## if calculated previously 
## - return cached matrix
## else 
##calculate using 'solve', save, and return
cacheSolve <- function(x, ...) {
        ## get inverse of x
        inv <- x$getinverse()
        ##if inverse already calculated return cached matrix
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)          
        }
        ##if not get the input matrix
        data <- x$get()
        ## calculate inverse
        inv <- solve(data, ...)
        ## set the inverse
        x$setinverse(inv)
        ##return the inverse
        inv
}
