## cacheSolve should find matrix inverse, first checking if the value has been cached by makeCacheMatrix

## 'makeCacheMatrix' should cache values of matrix and its inverse; given matrix it should return a list
##    - set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x<<-y
    I<<-NULL
  }
  get <- function() x
  setinv <- function(inv) I<<-inv
  getinv <- function() I
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## 'cacheSolve' should get the inverse of a matrix x, by first checking if the value has been cached, and if not, computing it using solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## x<-makeCacheMatrix(x)
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data)
  x$setinv(I)
  I
}
