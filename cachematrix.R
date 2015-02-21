## For the makeCacheMatrix
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse matrix
## Get the value of the inverse matrix

makeCacheMatrix<-function(x=matrix()) { 	
	inv=NULL				
	set.matrix=function(y) {		
		x<<-y
		inv<<-NULL
	}
	get.matrix=function() x
	set.inv=function(solve) inv<<-solve
	get.inv=function() inv
	list(set.matrix=set.matrix, get.matrix=get.matrix,
		set.inv=set.inv, get.inv=get.inv)
	 
}
## For cacheSolve function 
## The function will first check if the inverse matrix already has been computed, if not than it will been
## caluculated. If the invese matrix already exsist than it will get the inverse matrix from cached data. 

cacheSolve <- function(x, ...) {
    inv=x$get.inv()    ## Return a matrix that is the inverse of 'x'
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat<-x$get.matrix()
	inv=solve(mat, ...)
	x$set.inv(inv)
	inv
}

MCM		<-makeCacheMatrix(matrix(1:4,2,2)) ## read in matrix


## cacheSolve(MCM) ##   		[,1] [,2]
			 ##		[1,]   -2  1.5
			 ##		[2,]    1 -0.5


## cacheSolve(MCM) ##
			 ##		getting cached data
     			 ##			[,1] [,2]
			 ##		[1,]   -2  1.5
			 ##		[2,]    1 -0.5

