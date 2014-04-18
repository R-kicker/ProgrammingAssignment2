## function `makeCacheMatrix` produces special list
## with ability to synchronize with cached inverse matrix
## Short comment describing steps of the functions written
## inside function body just for peers

makeCacheMatrix <- function(x = matrix()) {
  cacheInv <- NULL
  setVal <- function (y) {
    # "copy" new matrix to "special matrix" object
    # use <<- operator to get data from parent space
    # that was passed as argument `x`
    x <<- y
    # consequently reset cache
    cacheInv <<- NULL
  }
  # just return matrix itself - need for access
  # to data as object of class that it had initially
  getVal <- function() x
  # set the calculated inverse matrix to cached variable
  setInv <- function(inv) cacheInv <<- inv
  # get the inverse matrix from cached variable
  getInv <- function() cacheInv
  # finally return special list
  list(setVal = setVal,
       getVal = getVal,
       setInv = setInv,
       getInv = getInv)
}


## function `cacheSolve` returns a matrix that is the inverse of 'x'
## NB! assumption: `x` must be a `special matrix object` created by
## `makeCacheMatrix` function from always invertible matrix,
## neither is being checked

cacheSolve <- function(x, ...) {
  mInv <- x$getInv()
  if(!is.null(mInv)) {
    message("Getting cached matrix:")
    return(mInv)
  } else {
    message("Calculating new inverse matrix. Operation may take time ...")
    mInv <- try(solve(x$getVal(), ...))
    x$setInv(mInv)
    message("Done")
    mInv
  }
}
