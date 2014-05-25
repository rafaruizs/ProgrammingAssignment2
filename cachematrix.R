## This function creates a list of 4 subfunctions: set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        ## The function set assigns the value of the matrix
        set <- function(y) {
                x <<- y ## Allows the subfunction to alter the x and i variables in the parent environment, 
                        ## the makeCacheMatrix() function itself
                i <<- NULL
        }
        ## The subfunction get take the stored matrix in the variable x and returns it
        get <- function() x
        ## The setinverse subfunction stores the value of the result for use it as cache later
        setinverse <- function(solve) i <<- solve
        ## The getinverse subfunction calls the value previously stores by the function setinverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function gets the inverse of a matrix using the function solve
## If the inverse has been calculated before, the function extract the value from
## the "cache" and informs to the user that it is using the previous calculted values
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()                    ## Assign the values of the function 
        if(!is.null(i)) {                      ## If the values already exists then extracts and return the values
                message("getting cached data")
                return(i)
        }
        data <- x$get()                        ## Save the value of the function get
        i <- solve(data, ...)                  ## calculate the inverse of the matrix and save it
        x$setinverse(i)                        ## Save the values using the function setinverse
        i                                      ## Output the result of the calculation or the old value
}
