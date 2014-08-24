##  makeCacheMatrix is going to give as output a list
##  containing two matrix, the original one and it is inverse
##  Mostly I am taking the overal approach of example from the exercise before
##  Names are just changed in makeCache matrix

makeCacheMatrix <- function(x = matrix()) {
  
 ## set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set value of the inv matrix
  setinv <- function(minv) inv <<- minv
  ## get the value of the inv matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## similar to example given, apart from the fact
  ## that solve is used to assign m
  m <- x$getinv()
  ## test if an inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  ## calculate the inverse:
  ## first copy the data from x to be sur not to destroy x
  data <- x$get()
  ##generate the inverse matrix
  m <- solve(data, ...)
  ##assign it
  x$setinv(m)
  ##tadaaam here it comes
  m
  ## thanks for reviewing
  ## you should stop reading now if you are busy
  ## you know you are lucky to review typed code
  ## my writing is horrible
  ## as you made it this far
  ## do you know why you never see elephants hiding in the trees?
  ## ...
  ## because they are very good at it! 
}