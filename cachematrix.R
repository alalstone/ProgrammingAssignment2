# cachematrix.R contains functions that make use of the cache and take advantage
# of scoping rules in R. makeCacheMatrix creates a list object with functions to 
# get and set the the values and inverse of a square invertable matrix. cacheSolve 
# acts on a list created using makeCacheMatrix and uses the solve() fucntion to 
# calculate the matrix inverse and store the value in the cache, if the 
# inverse already exists in the cache it is retrived saving computation time

#  makeCacheMatrix return a list for input matrix x with functions to:
#  1.set the value of the matrix
#  2.get the value of the matrix
#  3.set the inverse of the matrix
#  4.get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve looks for the inverse of a 'matrix' created using makeCacheMatrix 
## in cached data, if the inverse exists it returns it, if not it calculates 
## the inverse, which is then cached and returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
