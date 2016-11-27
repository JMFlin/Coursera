makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmat <- function(solve) m <<- solve
    getmat <- function() m
    list(set = set, get = get, setmat = setmat, getmat = getmat)
}

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmat()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmat(m)
    m
}


mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)

mat <- matrix(data = c(0.4,-0.2,-0.7,0.6), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)


m <- matrix(c(-1, -2, 1, 1), 2,2)
x <- makeCacheMatrix(m)
x$get()
inv <- cacheSolve(x)
inv
