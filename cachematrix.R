## This function creates a matrix object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {      ##This changes the matrix stored in the function.
      x <<- y
      s <<- NULL
    }
    get <- function() x       ##This gets the value of the matrix
    setinverse <- function(inverse) s <<- inverse  ##This stores the value of the input
    getinverse <- function() s  ##Returns the value of the input
    
    list(set = set, get = get,
    setinverse = setinverse, getinverse = getinverse)  ##Makes sure the object has all 4 functions
  }



## This function will compute the the inverse of the matrix function above

cacheSolve <- function(x, ...) {
        s <- solve(x)
          
        if(!is.null(s)) {  #Verifies the value of s and makes sure it's not NULL
          message("getting cached data")
            return(s)  ## Returns the value of s
  }
    data <- x$get()  ## Stores the matrix
    s <- solve(data, ...)  ##Calculates the inverse of the matrix
    x$setinverse(s)  ## Stores the inverse
    
    s
}
