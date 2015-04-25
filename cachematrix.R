## The overall obejvtive of this program to reduce matrix inverse calculation 
## time by storing object value in cache and provide fast response when asked
## repeatedly
## There two functions 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

## Define list of function for set, get and inverse
makeCacheMatrix <- function(x = matrix()) {
                inv <- solve(x)
                set <- function(y){
                x <- y
                inv <<- solve(x) 
                }
                get <-  function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
  
                list (set = set, get = get,setinverse = setinverse, 
                getinverse = getinverse)
}


## Check matrix inverse if already calculated if not calculate and return inverse matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(x)) {
    
        message("getting cached inverse matrix")
    
        return(inv)
         }
       mat_new <- x$get()
       inv <- solve(mat_new)
        x$setinverse(inv)
       inv
}
