## The following 2 functions generate a special matrix and caches its inverse
## Function 1, is intended to create the special matrix and store it in "m"
## Function 2, calculates the inverse only if it hasn't been calculated already

## Function 1
## In order to run and test the function below, generate the matrix as shown next:
##  m <- makeCacheMatrix(matrix(c(335,245,263,677),nrow=2,ncol=2))

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL					#Set the inverse to NULL for future values
        set <- function(y) {				#Defines a function to set the matrix
                x <<- y					#x is set to a new matrix which is y
                m <<- NULL				#reset the inverse (m) to NULL
        }
        get <- function() x				#Here the function returns the vector which is x
        setsolve <- function(solve) m <<- solve 	#sets m, to the inverse by using solve
        getsolve <- function() m			#returns the inverse contained in m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)			#returns the functions just defined above
}

## Function 2
## Execute and run the second function as shown next:
## cacheSolve(m)
## Since Function 2, returns m...If this is the first time you run cacheSolve, you will see the following
##             [,1]         [,2]
## [1,]  0.004169746 -0.001619857
## [2,] -0.001508992  0.002063316
## If cacheSolve has already being run, then instead of calculating the inverse, the cache inverse will be used and shown as below
## getting inverse cache data
##             [,1]         [,2]
## [1,]  0.004169746 -0.001619857
## [2,] -0.001508992  0.002063316



cacheSolve <- function(x, ...) {
        m <- x$getsolve()				#calculates the inverse
        if(!is.null(m)) {				#if m constain a calculated inverse already, then pull what's on cache
                message("getting inverse cache data")	#message to user that cache data is being pulled
                return(m)				#return the cache inverse data
        }
        data <- x$get()					#assign matrix to data
        m <- solve(data, ...)				#apply solve to "data" and assign it to m
        x$setsolve(m)					#set solve m
        m						#return m
}
