#### Course: "Coursera - R Programming"
#### Project: Programming Assignment 2: Lexical Scoping 

# This function creates a matrix object.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse matrix
    inv_mtx <- NULL
    
    # This is the setter function
    set <- function(x) {
        mtx <<- x;
        inv_mtx <<- NULL;
    }
    
    # This is the getter function
    get <- function() return(mtx);
    
    # This is the inverse setter function
    setinv <- function(inv) {
        inv_mtx <<- inv;
    }
    
    # This is the inverse getter function
    getinv <- function() {
        inv_mtx;
    }
    
    # Return the matrix
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


# This function calculate the inverse matrix of the given matrix x, 
# created by the makeCacheMatrix function.
# If the inverse matrix has been calculated, it returns the inverse matrix,
# and saves time from re-calculating every time the function is called.
cacheSolve <- function(x, ...) {
    # Return the inverse matrix
    inv_mtx <- x$getinv()
    
    # Return the previously calculated data.
    if(!is.null(inv_mtx)) {
        message("The inverse has been calculated. Loading data...")
        return(inv_mtx)
    }
    
    # Calculate the inverse matrix
    data <- x$get()
    inv_mtx <- solve(data, ...)
    
    # Cache the inverse matrix
    x$setinv(inv_mtx)
    
    # Return the inverse matrix
    inv_mtx
}
