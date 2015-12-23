## EXAMPLE 1:
## Each time new_counter is run, it creates an environment, initialises the counter i in this environment, 
## and then creates a new function.
new_counter <- function() {
  i <- 0
  function() {
    # do something useful, then ...
    i <<- i + 1    ## '<<' refers to the parent environment var i
    i
  }
}

testExample1 <- function(){
	f <- new_counter()
	print( f() )
	print( f() )
	print( f() )
	f <- new_counter()
	print( f() )
}



## EXAMPLE 2: 
## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## The following function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the 
## setmean function.
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("*** getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

testCacheMean <- function(){
	a <- makeVector(c(1,2,3,4))
	print(a$getmean())
	print(cachemean(a))
	print(cachemean(a))  ## this time we don't calculate the mean we used the cached value
	b <- makeVector(c(1,2,3,555))
	print(cachemean(b))  ## new calculation
}