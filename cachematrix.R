## The idea is to create a matrix an then cache its inverse with a function created by us.

## Function which creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL
    set <- function(y){
      x <<- y
      inversa <<- NULL
    }
    get <- function() {x}
    setInversa <- function(inverse) {inversa <<- inverse}
    getInversa <- function() {inversa}
    list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}


## This function will create the inverse of the matrix created above

cacheSolve <- function(x, ...){
  inversa <- x$getInversa()
  if(!is.null(inversa)){
    message("cached data")
    return(inversa)
  }
  matriz <- x$get()
  inversa <- solve(matriz,...)
  x$setInversa(inversa)
  inversa
}

