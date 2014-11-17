## ***************** Programming Assignment2 ************************
## The objective of this assignement is to gain more in depth understanding of the
## concept of Lexical Scoping. It also provide the understanding of how Object-Oriented
## programming can be implemented in R.
## The stated problem was to write functions which could help a user perform
##  following tasks:
##     1)  create a special matrix object which could cache it's inverse.
##     2)  computes the inverse of the created matrix and cache the inverse value.
##     3)  if the inverse has already been caluculated(cached) and the original matrix
##         has not been modified then the function should be able to retrieve the 
##         cached inverse value instead of recomputing the inverse.
##     4)  There should be ability to modify(set) either the original matrix and 
##         also the inverse matrix.
##
##
##    The following function creates a special matrix object (not just a matrix).
##    It takes in a normal matrix object and caches its value. 
##    it also initializes the inverse value to be NULL.
##    it creates the following four functions which are associated with the 
##    created object but not executed.
##    the four functions are following:
##    set()   sets or modifies the value of the original matrix
##    get()   gets the value of the original matrix
##    setinverse()  It modifes and caches the inverse value.
##    getinverse()  It caluculates, caches, and returns the inverse value
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL               #  inverse value is set to NULL
  set <- function(y) {
    x <<- y               #  the original matrix is cached
    m <<- NULL            #  the cached inverse value is set to NULL again.
  }
  ## the following functions are not executed when this function is called.
  ## these will be used when the object created by this function is passed
  ## in the function cacheSolve()
  
  get <- function() x     #  it returns the original matrix
  setinverse <- function(solve) m <<- solve   # it calculates and retruns the 
  # inverse value of the matrix
  getinverse <- function() m                  # it returns the cached inverse value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##   The following function takes in a specially created matrix object
##   ( the one created by the above function makeCacheMatrix) and calculates the inverse
##     value of the matrix. If inverse values are already cached then it returns the
##     cached inverse value instead of re-calculating it.
##     if inverse value is not cached previously then it calculates the inverse value
##     and caches it for future use.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix th at is the inverse of 'x'
  invmatrix <- x$getinverse()      # gets the cached inverse value
  if(!is.null(invmatrix)) {        # if the inverse value is cached previously
    message("getting cached data") # it announces that the cached value is retrieve
    return(invmatrix)              # returns the cached inverse value
  }
  data <- x$get()                  # if inverse value is not cached then it gets the
  # original matrix 
  invmatrix <- solve(data, ...)    # caluculates the inverse value by calling solve
  x$setinverse(invmatrix)          # caches the inverse value for future retrieval
  invmatrix                        # returns the inverse value
}
