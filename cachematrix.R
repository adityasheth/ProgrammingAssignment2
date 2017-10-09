# Caching Inverse of a Matrix

# Example usage
# mat1<- matrix(c(1, 2, 3, 0), nrow = 2, ncol = 2, byrow = TRUE)
# mat1
# mat2 <- makeCacheMatrix(mat1)
# cacheSolve(mat2)
# [,1]       [,2]
# [1,]  0.0  0.3333333
# [2,]  0.5 -0.1666667
# cacheSolve(mat2)
# getting cached data
# [,1]       [,2]
# [1,]  0.0  0.3333333
# [2,]  0.5 -0.1666667

## Creating a matrix which is capable of cachching its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
  
}


## The following function calculates the inverse of the matrix.
# If the inverse has already been calculated then it returns the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  return(inv)      

}
