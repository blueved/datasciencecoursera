## Put comments here that give an overall description of what your
## functions do
## The assignment is regarding lexical scoping and caching functions that may require a long computation time. 
## Specifically I am using solve() to find the inverse of a matrix and cache it using a free floating variable. 
## I am returning an error as described below.

## First I stored a function in a variable a<-makeCacheMatrix()Then I run a$set(matrix(1:4,2,2)to store a matrix

## When I run cacheSolve(a) I get Error in as.vector(x, mode) : cannot coerce type 'closure' to vector of type 'any'
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
      message("getting cached data")
      return(m)
    }
    matrix <- x$get() 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
