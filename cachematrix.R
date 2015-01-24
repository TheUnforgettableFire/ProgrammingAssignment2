## makeCacheMatrix function
## The makeCacheMatrix function will create a matrix object that can cache its inverse.

## x is an empty matrix
makeCacheMatrix <- function(x = matrix()) {
  ##  m is the inverse matrix (NULL)
  m <- NULL 
  
  set <- function(y){
    ## set assigns the argument to x
    x <<- y
    ## when set is called, Inverse matrix should be assigned NULL
    m <<- NULL
  }
  ## get returns the matrix
  get <- function() x
  
  ## setInverse overwrites older value of m and assigns the argument to Inverse
  setInv <- function(solve) m <<- solve
  
  ## getInv returns the Inverse
  getInv <- function() m
  
  ## this is a list of set, get, setInv and GetInv functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}

## cacheSolve function
## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  ## Assigns most recent value for the inverse
  m <- x$getInv()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
    ## If m is NOT null i.e. previously calculated then cacheSolve returns the value of m  
  }
  ## If m is NULL, then you get matrix x and calculate inverse with solve()
  data <- x$get()
  m <- solve(data, ...)

  ## Set Inverse to the newly calculated value
  x$setInv(m)    

  ## Returns the new Inverse value
  m 
}