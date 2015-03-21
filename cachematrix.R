## Put comments here that give an overall description of what your
## functions do
##
## There are 2 functions that are written as part of this assignment
## 1. makeCacheMatrix
##    This function actually instantiates an R object that is slightly
##    different from the R Matrix object in that it also includes some 
##    special functions that make use of the Lexical Scoping capabilities
##    of R. 
##
##    Two main variables are defined within the makeCacheMatrix environment.
##    a. x:   This x variable is actually a matrix object residing within the 
##            scope of the makeCacheMatrix environment.
##    b. m:   This m variable will be used to hold the INVERSE matrix of 
##            x matrix object defined above. Again, this is within the
##            scope of the makeCacheMatrix environment.
##
##    Here is a description of the functions that make use of the above
##    two variables.
##    a. GET: This function returns the matrix object as is defined currently
##            in the makeCacheMatrix environment, not the calling environment.
##            This will generally return what is contained in x
##    b. SET: This function sets a matrix object into the makeCacheMatrix
##            environment, not the calling environment. However, the values used
##            do come from the calling environment.
##            Essentially it will set x to the passed matrix and m to NULL.
##    c. SETINVERSE: This function can receive a matrix passed to it 
##                   and assign it 
##                   to the m object. Generally, you will compute the
##                   inverse of a given matrix and then get it assigned to m.
##    d. GETINVERSE: This function will print out the value of m.
##                   If when this function is called m is NULL, then
##                   a full run of the SOLVE R function is invoked and this 
##                   full computation is stored in m for future use.
##                   If when this function is called m is NOT NULL, then
##                   the cached value for this particular input matrix
##                   exists within the makeCacheMatrix environment and that
##                   is returned WITHOUT invokeing SOLVE.
##
## 2. makeCachcMatrix
##    This function will compute the inverse of a given matrix by first
##    checking to see if it had already computed the inverse in prior
##    invocations of itself. If yes, then it will just return that object
##    from its own environment, and not the calling environment. If no, then
##    it will call the SOLVE R function to compute the inverse, and then
##    automatically cache this in its environment.
##
##   Below is an example run of how to run the code: This shows the computation
##   of the inverse of a 3x3 matrix by instantiating the matrix, showing the
##   list of functions, and then once computing the inverse and once getting 
##   the inverse using the cache.

# > c <- makeCacheMatrix(matrix(c( 1,2,3,0,1,4,5,6,0), nrow=3, ncol=3, byrow=TRUE))
# > c
# $set
# function (y) 
# {
    # x <<- y
    # m <<- NULL
# }
# <environment: 0x0000000027276bc8>
# 
# $get
# function () 
# x
# <environment: 0x0000000027276bc8>
# 
# $setinverse
# function (inverse) 
# m <<- inverse
# <environment: 0x0000000027276bc8>
# 
# $getinverse
# function () 
# m
# <environment: 0x0000000027276bc8>
# 
# > d <- cacheSolve(c)
# > d
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > e <- cacheSolve(c)
# getting cached data
# > e
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
#  > 

##
##
## Write a short comment describing this function
###################################################################
## This makeCacheMatrix function is used to instantiate an R object
## that is built upon the R MATRIX object but adds instantiating 
## functions such as GET, SET, and also has functions to compute
## inverse of the given matrix by using lexical scoping to provide
## inverse computation caching.
###################################################################
makeCacheMatrix <- function(x = matrix()) {
	## m variable will hold inverse
        m <- NULL
	## set function initializes m to NULL and copies the calling envirnment
	## x matrix to the function environment x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	## get function returns the matrix x
        get <- function() x
	## setinverse function allows the setting of m in function environment 
	## by passing the inverse as an argument.
        setinverse <- function(inverse) m <<- inverse
	## getinverse just returns the value of m, which is NULL if new matrix, 
	## and contains the inverse if it was computed before.
        getinverse <- function() m
	## This list of functions is returned to the calling environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

###################################################################
## This cacheSolve function computes the inverse of a passed R object
## instantiated as a super object of type R Matrix. This object as
## shown above contains a list of functions and a couple of matrix 
## variables. It will check the GETINVERSE function to see if the 
## internal m variable already holds an inverse, and if yes, it will
## return that as the inverse as it has been cached. If no, it will
## call the SOLVE R function to compute the inverse and populate 
## cache automatically as this will be stored in the function's
## environment and not directly available to the calling environment.
###################################################################
cacheSolve <- function(x, ...) {
        ## Read the object's getinverse function into m
	m <- x$getinverse()
	## if above read is NOT NULL, then we already computed this so
	## just return the cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	## If we got here, this is a newly encountered matrix with 
	## new values so we need to compute inverse and set it into
	## our cache in our function environment
        data <- x$get()
        m <- solve(data, ...)
	## cache the inverse that we just computed
        x$setinverse(m)
	## Return the inverse
        m
}
