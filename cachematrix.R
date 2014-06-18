## This pair of functions can be used to create a matrix, find its inverse,
## and cache that inverse.
## This makes it unnecessary to recalculate the inverse every time.
## If the inverse has already been calculated, and the matrix hasn't
## changed since, the cached inverse will simply be used.

## This function creates an object that is a sort of super-matrix.
## It is really a set of four functions, which...
# give the matrix a value, which also wipes out any cached inverse (set)
# retrieve the matrix's value (get)
# set the value of the inverse (setsolve)
# retrieve the value of the inverse (getsolve)


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function takes the inverse of a "super-matrix" 
## created by makeCacheMatrix()
## First, it checks whether a cached inverse already exists.
## If so, it retrieves it, using the supermatrix's getsolve() function.
## If not (i.e., if getsolve() returns NULL),
## the function calculates the inverse, using solve(), and then 
## caches it using the supermatrix's setsolve() function.
# This function assumes we'll only be dealing with matrices that are
# actually invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## Let's test it (thanks to Adam Gruer's explanation on the forums):

mat <- makeCacheMatrix() #mat is now a list of 4 functions
mat$set(matrix(c(2,2,3,2),nrow=2,ncol=2)) #use mat's set() function to give it numbers
mat$get() #use mat's get() function to see what we just set it to 
cachesolve(mat) #cache and return mat's inverse
cachesolve(mat) #try it again--now you get a message 'getting cached data'


