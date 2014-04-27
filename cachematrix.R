#The first function, makeCacheMatrix creates a special "Matrix", 
#which is really a list containing a function to
#	set the value of the matrix
#	get the value of the matrix
#	set the value of the matrix's inverse
#	get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
	#this variable will hold the inverse of the matrix
    inv <- NULL 
	
	#set the value of the matrix and clear the value of the matrix's inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
	
	#get the matrix
    get <- function() x
	
	#set the value of the matrix's inverse to what is passed
    setinverse <- function(inverse) inv <<- inverse
	
	#get the value of the matrix's inverse
    getinverse <- function() inv
	
	#construct the list of the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}

#The following function cacheSolve calculates the inverse of the special 
#"matrix" created with the above function. However, it first checks to see
#if the inverse has already been calculated. If so, it gets the inverse from
#the cache and skips the computation. Otherwise, it calculates the matrix's 
#inverse and sets the value of the inverse in the cache via the setinverse
# function.

cacheSolve <- function(x, ...) {
    #get the value of the inverse of the matrix
    inv <- x$getinverse()
    
    #if it is not null return the computed value from the cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #otherwise get the matrix and compute its inverse
    mymatrix <- x$get()
    inv <- solve(mymatrix, ...)
    
    #cache the inverse
    x$setinverse(inv)
    
    #return the inverse
    inv
}

