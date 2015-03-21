######################################################################################################
##
## The functions below allow to cache the inverse of a matrix this way avoiding having to recalculate
## if the value is still in the cache. Only when the cache is empty or the cache contains the inverse
## of another matrix the inverse will be calculated and stored in the cache.
## Function <makeCacheMatrix> makes the cache environment for the matrix and function <cacheSolve>
## returns the inverse of the matrix returned by the makeCacheMatrix, either recalculated (and stored
## in the cache) or just taken from the cache. 
##
######################################################################################################

######################################################################################################
########################################### makeCacheMatrix ##########################################
######################################################################################################
## 
## For the matrix (in the argument) of the function, an empty cache is being created for storing the
## matrix and its inverse matrix. It returns a list of functions that, when calling from outside this
## environment, will be applied on this particular matrix environment, like returning the matrix value,
## setting the matrix value, getting the cache and setting the cache.
## 
######################################################################################################
makeCacheMatrix <- function(x = matrix()) {
	
  	lCache <- NULL
	
	## This fucntion will return the value of the matrix ##
	fgetMat <- function() { x }
  
  	## This function will set the value of the matrix and clear the cache ##
  	fsetMat <- function(y = matrix()) {
      		x <<- y
      		lCache <- NULL
  	}

	## This function will return the cache, i.e. when not empty ##
  	## the last stored matrix and its inverse ##
	
	fgetCache <- function() { lCache }

	## This function will store the matrix (argMat) ## 
  	## and its inverse (argInv) in the cache ##
	
	fsetCache <- function(argMat, argInv) {
	  	lCache <<- list(mat = argMat, inv = argInv)
	}

	## Returning a list of functions (labels are function names without 'f') ##
	list(getMat = fgetMat,
       		setMat = fsetMat,
	   	getCache = fgetCache, 
	   	setCache = fsetCache)
}
######################################################################################################
############################################## cacheSolve ############################################
######################################################################################################
## 
## This function will use the functions of the matrix environment returned by the makeCacheMatrix
## function and given as argument in the cacheSolve function, to get the cache and check if either the
## inverse is to be calculated or if it can simply be taken from the cache. In the first case it will    
## also store the new value in the cache (together with the matrix).
##
######################################################################################################
cacheSolve <- function(x, ...) {
	
  	## Get cached matrix and inverse matrix, if nothing cached yet the result will be NULL ##
	## (Note the cache is a list of two matrices, the matrix and its inverse) ##
  	curCache <- x$getCache()
	
  	## Get current matrix ##
	curMat <- x$getMat()

	## Check if something is cached ##
	if(!is.null(curCache)) {
    		## the cache is not empty, now check if the current matrix is the one in the cache ##
    		if(identical(curMat, curCache$mat)) {
			## no invert to be done, just return the cached inverse matrix ##
        		## print("matrix not changed so cached inverse returned")
        		return(curCache$inv)
		} 
	}
	
  	## Nothing cached or matrix has changed: Invert matrix and store in return field ##
  	retMat <- solve(curMat)
  
  	## Store matrix and its inverse in the cache ##
  	x$setCache(curMat, retMat)

	## Return the inverse matrix ##
	return(retMat)
}		 
######################################################################################################