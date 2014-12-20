## Make cache function is realization of a "wrapper" design pattern. It 
## creates a container around the original matrix, which allows to cache computed values
## to avoid recomputation every time.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                                        # Initially matrix inverse is empty
  set <- function(new_matrix){
    inverse <<- NULL                                     # If matrix object changes - clear the cached inverse
    x <<- new_matrix                                     # Set new matrix
  }
  get <- function() x                                    # Get matrix object captured by closure
  setInverse <- function(new_inverse) inverse <<- new_inverse # Use <<- operator to modify cached inverse (closure)
  getInverse <- function() inverse
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse ) # return list of possible operations on matrix
}


## This function allows to perform caching / computing operations on a matrix wrapper
## created bu the makeCacheMatrix. Parameters are:
## x - matrix wrapper - output of the makeCacheMatrix funciton
## ... - other parameters, that can be passed to R built-in function "solve"

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()                             # try to get existing inverse
  if(!is.null(inverse)){                                # if inverse is already there - return it
    message("*** getting cached data ***")
    return(inverse)
  }
  message("*** compute new inverse and save to cache ***")
  computed_inverse <- solve(x$get(), ...)               # if not - compute new inverse getting the cached matrix object
  x$setInverse(computed_inverse)                        # save cached inverse and return it
  computed_inverse 
}

## Below examples of working function:


# Create random matrix:
# ---------------------------
# m <- diag(x = runif(n = 9, min=1, max=5), nrow=3, ncol=3)


# example output (randomized):
# ---------------------------
# m
#     [,1]     [,2]    [,3]
#[1,] 3.642894 0.000000 0.00000
#[2,] 0.000000 3.482184 0.00000
#[3,] 0.000000 0.000000 3.54967

# Create matrix wrapper:
# ---------------------------
# matrix_wrapper <- makeCacheMatrix(m)


# Wrapped object should look the same as the original matrix:
# ---------------------------
# matrix_wrapper$get()
#      [,1]     [,2]    [,3]
# [1,] 3.642894 0.000000 0.00000
# [2,] 0.000000 3.482184 0.00000
# [3,] 0.000000 0.000000 3.54967


# Inverse should be empty in the beginning:
# ---------------------------
# matrix_wrapper$getInverse()
# NULL


# Calculate cached inverse - for the first time value is really computed:
# ---------------------------
# cacheSolve(matrix_wrapper)
# *** compute new inverse and save to cache ***
#       [,1]      [,2]      [,3]
# [1,] 0.274507 0.0000000 0.0000000
# [2,] 0.000000 0.2871761 0.0000000
# [3,] 0.000000 0.0000000 0.2817163


# Calculate cached inverse - for the second time value is taken from cache:
# ---------------------------
# cacheSolve(matrix_wrapper)
# *** getting cached data ***
#     [,1]      [,2]      [,3]
# [1,] 0.274507 0.0000000 0.0000000
# [2,] 0.000000 0.2871761 0.0000000
# [3,] 0.000000 0.0000000 0.2817163

# One can manually proove that calculating the inverse normally would give the same result