## Put comments here that give an overall description of what your
## functions do
## Create a square invertible matrix
myMatrix <- matrix(c(2, 3, 3, 4), nrow = 2, ncol = 2)

## Use makeCacheMatrix to create a special matrix object that can cache its inverse.
## This object is a list containing functions to set and get the matrix and its inverse.
cachedMatrix <- makeCacheMatrix(myMatrix)

## Compute the inverse of the matrix for the first time using cacheSolve.
## Since the inverse is not yet cached, cacheSolve will compute it and store it in the cache.
inverseMatrix <- cacheSolve(cachedMatrix)
print("Computed Inverse:")
print(inverseMatrix)

## Calling cacheSolve again will retrieve the inverse from the cache,
## rather than recomputing it. You should see a message indicating that cached data is used.
cachedInverseMatrix <- cacheSolve(cachedMatrix)
print("Cached Inverse:")
print(cachedInverseMatrix)
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions to set and get the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # inv will hold the cached inverse of the matrix
  inv <- NULL
  
  # set: Assigns a new matrix to x and resets the cached inverse to NULL.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get: Returns the current matrix.
  get <- function() x
  
  # setInverse: Caches the inverse of the matrix.
  setInverse <- function(inverse) inv <<- inverse
  
  # getInverse: Retrieves the cached inverse (if it exists).
  getInverse <- function() inv
  
  # Return a list containing the above four functions.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## It first checks if the inverse has already been computed. If so, it retrieves the cached value.
## Otherwise, it computes the inverse using solve(), caches the result, and then returns it.
cacheSolve <- function(x, ...) {
  
  # Attempt to retrieve the cached inverse.
  inv <- x$getInverse()
  
  # If the inverse is already cached, print a message and return it.
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # Otherwise, get the matrix from the object.
  data <- x$get()
  
  # Compute the inverse of the matrix using the solve() function.
  inv <- solve(data, ...)
  
  # Cache the computed inverse for future use.
  x$setInverse(inv)
  
  # Return the computed inverse.
  inv
}

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It returns a list containing functions to set and get the matrix and its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the variable to store the inverse
  
  # set: Assigns a new matrix to x and resets the inverse cache to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get: Returns the current matrix stored in the object
  get <- function() x
  
  # setInverse: Stores the computed inverse in the cache
  setInverse <- function(inverse) inv <<- inverse
  
  # getInverse: Retrieves the cached inverse, if available
  getInverse <- function() inv
  
  # Return the list of functions to interact with the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been computed and the matrix has not changed, it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to retrieve the cached inverse
  
  if (!is.null(inv)) {   # If the inverse is already cached, return it
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()        # Get the matrix from the object
  inv <- solve(data, ...)  # Compute the inverse of the matrix using solve()
  x$setInverse(inv)      # Cache the computed inverse for future use
  
  inv  # Return the inverse
}