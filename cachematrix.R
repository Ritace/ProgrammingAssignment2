## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## set matrix, get the determinant, compute the inverse, check if set
  y <- x
  inv <- NULL
  setMatrix<-function(x) y <<- x
  getMatrix<-function() y
  getDet<-function() c(nrow(y),ncol(y))
  findInverse<-function(){
    det<-getDet()
    if (det[1]==det[2]){
      inv <<- solve(y)
      } else {
      print("Not a square matrix")
      inv <<- NULL
    }
  }
  getInverse<-function() inv
  list(setMatrix=setMatrix,getDet=getDet,getMatrix=getMatrix,findInverse=findInverse,getInverse=getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## see if inverse if found
  if(is.null(x$getInverse())){
    print("here")
    x$findInverse()
    return(x$getInverse())
  } else {
    print("using cache")
    return(x$getInverse())
  }
}
sample_array = array(c(1,2,3,4),dim = c(2,2))

cache_matrix<-makeCacheMatrix(sample_array)
cache_matrix$findInverse()
print(cacheSolve(cache_matrix))

