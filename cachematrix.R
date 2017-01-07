## This code can be used as an "R" function to create inverse of a matrix. Once the inverse is calculated, it can also be
## retrieved from the cache and returned as result.
    
## "makeCacheMatrix" function creates a list containing a function to

## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inverse
## 4. Get the value of the matrix inverse
    
makeCacheMatrix <- function(x = matrix()) 

    {
            m <- NULL
            set <- function(y) {
            x <<- y
            m <<- NULL
                               }
    
            get <- function() x
            setinv <- function(inv) m <<- inv
            getinv <- function() m
            list(set = set, get = get, setinv = setinv, getinv = getinv)
    }
    
## The purpose of following function is to calculate the inverse of the vector created with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.
        
cacheSolve <- function(x, ...) 
{

        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
            if(!is.null(m)) {
            message("getting cached data")
            return(m)
            }
        
        data <- x$get()
            m <- solve(data,...)
            x$setinv(m)
            m
}