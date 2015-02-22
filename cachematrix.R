## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 1. define set the value of the matrix
# 2. define get the value of another matrix and reset inv
# 3. define setinv function to set inv
# 4. define getinv  function that returns inv
# 5. return list of methods

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y=matrix()) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() 
    x
  setinv <- function(inverse=matrix())
    inv <<- inverse
  getinv <- function()
    inv
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
# 1. calls getinv and assigns to i
# 2. if i[1,1] is not NA returns value of matrix i
# 3. if i[1,1] is NA - gets initial matrix and computes inverse by calling solve(matrix)
# 4. passes inverse matrix to x object and returns the value of inverse
# TODO: implement exception handler if inverse cannot be determine
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.na(i[1,1])){
    message("inverse cached")
    return(i)
  }
  mat<-x$get()
  i<-solve(mat)
    
  x$setinv(i)
  i
}
