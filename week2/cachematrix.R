## Build a customized matrix which provides cache
## [in] : invertible matrix
## [out]: a customized matrix which returns a list of 4 functions:
## - set
## - get
## - setmatrix
## - getmatrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	## set function
	set<-function(y){
		x<<-y       ## access the parent environment variable 'x'
		m<<-NULL	## access the parent environment variable 'm'
	}
	## source matrix accessor
	get<-function() x
	## calculates the inverse of the given matrix
	setmatrix<-function(solve) m<<- solve
	## inverse matrix accessor
	getmatrix<-function() m
	## returns a list
	list(set=set, get=get,
		setmatrix=setmatrix,
		getmatrix=getmatrix)
}

## Calculate the inverse of a given matrix 
## [in]: matrix to invert - we assume it is invertible, there is no checking here
## [out]: inverted matrix
cacheSolve <- function(x=matrix(), ...) {
	## check if there is cached value for this matrix
    m<-x$getmatrix()
    if(!is.null(m)){
      message("*** getting cached data")
      return(m)
    }
	## no cached value:
    matrix <- x$get() 		## get the matrix to invert
    m<-solve(matrix, ...)	## invert it
    x$setmatrix(m)			## cache the value 
    return(m)				## return the inverted matrix
}


## Test of the cache matrix 
unitTest <- function(){
	s <- matrix(1:4,2,2)	## test matrix, make sure it is invertible (det non null)
	mt <- makeCacheMatrix(s)## build our customized super matrix
	print('- first pass goes through calculation')
	print(cacheSolve(mt))   ## calculate the inverse
	print("- second pass should retrieve the cached value")
	i<-cacheSolve(mt)		## re-calculate the inverse (using the cached value this time)
	print("- product of both matrices should return identity matrix")
	i %*% s					## matrix product of source and inverted matrices
}