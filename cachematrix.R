## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set<- function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function(){
    x
  }
  setinverse <- function(inverse){
    i<<-inverse
  }
  getinverse<-function(){
    i
  }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  d <- x$get()
  m<-solve(d)%*%d
  x$setinverse(m)
  m
}
