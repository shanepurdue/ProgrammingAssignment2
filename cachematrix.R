
## makeCacheMatrix and cacheSolve are part of Assignment 2
##  for R Programming in the Coursera Data Science series
##  of courses


## makeCacheMatrix creates a pseudo matrix with functions to set 
##  and get a matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve(x)
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve checks to see if a makeCacheMatrix has a current
##  matrix inverse and returns it if so - if the matrix inverse
##  isn't current, it uses the solve function to invert the
##  matrix and returns the new inverted matrix

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setSolve(s)
  s
}
