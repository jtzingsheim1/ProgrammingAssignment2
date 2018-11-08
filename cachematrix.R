# This script is for the Coursera Data Science Course 2 Week 3 Programming assignment
# Below there are two functions which together can create a special object that stores a
## matrix and caches its inverse. An example of how these functions might be used is:
## cacheSolve(makeCacheMatrix(your_matrix)) where "your_matrix" is the matrix to invert

# The first function is called makeCacheMatrix, and it creates and returns a list of four
## functions for the matrix which can:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # This line creates the variable which will store the inverse matrix later,
    ## for now it is set to NULL
  set <- function(y) { # This line creates the first element of MakeCacheMatrix which is
    ## a function that can locally store whichever matrix is passed into it
    x <<- y # Locally stores the matrix passed in (y) in the variable x
    inv <<- NULL # Ensures inv is clear from previous values/uses
  }
  get <- function() x # This line creates the second element of MakeCacheMatrix which is
    ## a function to return the internal value of the matrix which is x as set above
  setInv <- function(inverted) inv <<- inverted # This line creates the third element of
    ## MakeCahceMatrix which is a function the can locally store the inverse which is
    ## calculated elsewhere and passed in
  getInv <- function() inv # This line creates the fourth element of MakeCacheMatrix
    ## which is a function the returnt he internal value of the matrix which is "inv" as
    ## set above
  list(set = set, get = get, setInv = setInv, getInv = getInv) # This line assembles the
    ## four functions created above into a list and it is the return of makeCacheMatrix
}

# The second function will take the makeCacheMatrix function with a matrix as input and
  ## it will return the inverse of the matrix, but before calculating the inverse it
  ## will check to see if the inverse has already been calculated and cached

cacheSolve <- function(x, ...) {
  inv <- x$getInv() # This line retrieves the value of inv from within makeCacheMatrix
  if(!is.null(inv)) { # This line checks if the value of inv is not NULL, if its not
      ## NULL then it retrieves the cached value of the inverse instead of calculating it
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # If the value of inv was NULL then the inverse will be calculated,
    ## first the matrix is retrieved from within makeCacheMatrix and stored in "data"
  inv <- solve(data, ...) # This line calculates the inverse and stores it in inv
  x$setInv(inv) # This line caches the inverse in makeCacheMatrix for future use
  return(inv) # The newly calculated inverse is returned
}