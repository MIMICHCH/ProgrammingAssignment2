

## create a "matrix" (actually it is a list containing four functions) which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <-function(inverse) inv <<- inverse
  getinverse <-function() inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}

## If the inverse has been calculated, cacheSolve will retrieve the inverse from cache 
## and give message "getting cache data".
## And if the inverse is not computed previously, the function will calculate the inverse of the "matrix" 
##created by function makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data,...)
  x$setinverse(inv)
  inv
        
}

