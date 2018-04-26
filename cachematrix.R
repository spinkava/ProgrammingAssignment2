## These functions attempt to save time computing the inverse of matrices by 
## creating a cache of the inverses that are already computed and calling 
## those values

## This function sets and gets the cached values for matrix inverses 

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
               x <<- y
               m <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) m <<- inverse
       getinverse <- function() m
       list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## This function returns the inverse of a matrix that is already computed, 
## or it computes the inverses for matrices not already computed

cacheSolve <- function(x) {
       m <- x$getinverse()
       if(!is.null(m)) {
               message("getting cached data")
               return(m)
       }
       data <- x$get()
       m <- solve(data)
       x$setinverse(m)
       m
}
