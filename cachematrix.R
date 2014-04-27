#The first function, makeVector creates a special "Matrix", 
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

## Write a short comment 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
