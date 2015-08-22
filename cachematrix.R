## These functions allow you to calculate the inverse of a square matrix  
## without spending time calculating an inverse known already. 

## The function "makeCacheMatrix" creates a cache which is formed by 
## four functions. "Set" allows you to put a matrix in the working enviroment.
## "Get" allows you to obtain the matrix' value. "Setsolve" allows you to put 
## the matrix' inverse in the working space. "Getsolve" allows you to obtain 
## the inverse's value. 

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
		set <- function(y){
				x <<- y
				inverse <<- NULL
		}
		get <- function() x
		setsolve <- function(solve) inverse <<- solve
		getsolve <- function() inverse
		list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
		
		

}


## The function "cacheSolve" obtains the matrix' inverse which you want from 
## the cache. If it was calculated before it returns this value without do any 
## calculation. If not, it calculates it and saves the result in the cache. 

cacheSolve <- function(x, ...) {
		inverse <- x$getsolve()
		if(!is.null(inverse)){
			message("getting cached data")
			return(inverse)
		}
		data <- x$get()
		inverse <- solve(data, ...)
		x$setsolve(inverse)
		inverse
}
