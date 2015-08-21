# this is the first function, used matrix function
# caching the inverse of a matrix rather than compute it repeatedly. The
# used two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to


makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# this is the second function
# the function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  matrixdata <- x$getmatrix()
  inv <- solve(matrixdata, ...)
  x$setinv(inv)
  return(inv)
  
}

