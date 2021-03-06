######################################################################################
#Rodrigo Carrasco
#set the value of the vector
#get the value of the vector
#Test
#a<-makeCacheMatrix()
# a$set(matrix(1:4, 2, 2))  
#cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) 
{
    mtx <- NULL
    set <- function(y) {
        x <<- y
        mtx <<- NULL     }
    get <- function() x
    setinverse <- function(inverse) mtx <<- inverse
    getinverse <- function() mtx
    list(
    set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
 
cacheSolve <- function(x = matrix(), ...) 
{
    mtx <- x$getinverse()
    if(!is.null(mtx)) {
        message("cached data")
        return(mtx)
    }
    dat <- x$get()
    mtx <- solve(dat, ...)
    x$setinverse(mtx)
    mtx
}
