makeCacheMatrix <- function(at = matrix()) { ##creation of matrix
	inver <- NULL   
	set <- function(y) {  
		at <<- y
		inver <<- NULL   }
	get <- function() at	 ##return matrix
	setinver <- function(inverse) inver <<- inverse  ##set matrix inverse
	getinver <- function() inver ##return inverse of matrix, if matrix inverse hasn't calculated yet then return NULL
	list(set = set,get = get, setinver = setinver,getinver = getinver)     ##list of functions
		
		
}

cacheSolve <- function(at) { ##function that calcule and return the inverse of matrix 'at'
	inver <- at$getinver() ## get the inverse
        if(!is.null(inver)) { 
                message("getting cached data")
                return(inver)
        }
        data <- at$get() ##if inverse has not been calculated then get data of ´at´ 
        inver <- solve(data) ##calcule inverse of data
        at$setinver(inver) ##set the inverse of object at
        inver	##return the inverse
}
