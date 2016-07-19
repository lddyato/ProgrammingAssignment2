## Lexical Scoping--caching the inverse of a matrix
## makeCacheMatrix creates a special "matrix"
## which is really a list containing a function to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y){
     x <<- y
     i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the mean of the special "vector" created with 
## the above function. However, it first checks to see if the mean has already been 
## calculated. If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in 
## the cache via the setmean function.
## this function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
i <- x$getinverse()
if(!is.null(i)){
    message("getting cached data.")
    return (i)}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}

## Sample
> Cmatrix <- makeCacheMatrix()
> x <- matrix(c(1, 1, 2, -1, 2, 0, 1, 1, 3), 3, 3, byrow=T)
>  Cmatrix$set(x)
> Cmatrix$get()
     [,1] [,2] [,3]
[1,]    1    1    2
[2,]   -1    2    0
[3,]    1    1    3
> Cmatrix$getinverse()
NULL
> cacheSolve(Cmatrix)
     [,1]       [,2]       [,3]
[1,]    2 -0.3333333 -1.3333333
[2,]    1  0.3333333 -0.6666667
[3,]   -1  0.0000000  1.0000000
