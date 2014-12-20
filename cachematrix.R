## The functions below are used to find the inverse of a matrix.

## In makeCacheMatrix, we will initialize a variable "m" in which ## the inverse matrix will be saved. The function get() is used ## to obtain a matrix whose inverse is to be found.
## CompInvMatrix() is used to the computed inverse matrix to "m"
## GetInvMatrix() will obtain the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) 
{
	
	m <- NULL
	get <- function() x
	CompInvMatrix <- function(InvMatrix) m <<- InvMatrix
	GetInvMatrix <- function() m

## The function returns a list of functions as an R object.

	list (get=get, CompInvMatrix=CompInvMatrix, GetInvMatrix=GetInvMatrix)

}


## The cacheSolve function inverses the matrix "x". First, it
## checks if the inverse has been found. If yes, it returns the 
## result and quits. If not, it calculates the inverse, saves to ## cached, then returns the inverse matrix.

cacheSolve <- function(x) 
{

m <- x$GetInvMatrix()

if(!is.null(m))
	{
		message("Inverse has been found. Getting inverse matrix.")
		return(m)
	}
	
else
	{
		message("No cached data found. Calculating inverse.")
		data <- x$get() ## obtains matrix from object x
		m <- solve(data) ## to find the inverse
		x$CompInvMatrix(m) ## assigns result to x
		message("Done.")
		return(m)
		
	}
}
