makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  setmatrix<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()
  xetinv<-function(inverse) inv<<-inverse
  getinv<-function()inv
  list(setmatrix=setmatrix, getmatrix=getmatrix,
       setinv=setinv, getinv=getinv)
}
##this function
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
