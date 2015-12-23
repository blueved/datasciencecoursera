## Put comments here that give an overall description of what your
## functions do
## The assignment is regarding lexical scoping and caching functions that may require a long computation time. 
## Specifically I am using solve() to find the inverse of a matrix and cache it using a free floating variable. 

## Stored a function in a variable a<-makeCacheMatrix()
## Run a$set(matrix(1:4,2,2)to store a matrix
## Run cacheSolve(a)
## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
	  m<-NULL
	  set<-function(y){
	  x<<-y
	  m<<-NULL
	}
	get<-function() x
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m
	list(set=set, get=get,
	   setmatrix=setmatrix,
	   getmatrix=getmatrix)
}

## Write a short comment describing this function
cacheSolve <- function(x=matrix(), ...) {
	## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
      message("*** getting cached data")
      return(m)
    }
    matrix <- x$get() 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

unitTest <- function(){
	mt <- matrix(c(1,2,3,4), 2, 2)
	cacheSolve(mt)
	cacheSolve(mt)
	
}