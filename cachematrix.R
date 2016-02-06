## This R program  provides function to perform fast matrix inversion using caching 
## The first time run will be slower , but once the data  inverse matrix is cached ,
## subsequent execution will be much faster as it will used cached data.

## Steps to run
## Set current working directory to the code file path

## setwd(path to current code)
## source("cachematrix.R")
## test() # first time , check statistics
 
## compare time taken in both runs. 
## Second time run will take less time
 
## This function creates a special "matrix" object  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invmatrix = NULL
        set = function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get = function() x
        setinvmatrix = function(inverse) invmatrix <<- inverse 
        getinvmatrix = function() invmatrix
        list(set=set, get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated in prevous run  (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## returns the inverse of the original matrix input to the calling function , here makeCacheMatrix()        
        invmatrix = x$getinvmatrix()        
        # reuse if the inverse is in cache
        if (!is.null(invmatrix)){
                # reuse from cache
                message("getting cached matrix")
                return(invmatrix)
        }        
        # else get inverse using R function solve
        matrixorig = x$get()
        inverse = solve(matrixorig, ...)        
        # set cache with result to be reused in next run
        x$setinvmatrix(inverse)        
        return(inverse)
}

## This funtion will test / validate the above functions .cacheSolve will be called 2 times and you can see the difference in execution time.
test = function(){
	## generate 100 X 100 matrix with random numbers
	testmatrix <- matrix(rnorm(10000),100,100)
	tempdata = makeCacheMatrix(testmatrix)	
	##first time run
	start = Sys.time()
    cacheSolve(tempdata)	
    executiontime = Sys.time() - start
    print(executiontime)
	## Second time run
	start = Sys.time()
    cacheSolve(tempdata)
    executiontime = Sys.time() - start
    print(executiontime)

	}
	
	
