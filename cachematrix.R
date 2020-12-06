makeCacheMatrix <- function(input = matrix()) {
        invert <- NULL
        set <- function(y) {
            input <<- y
            invert <<- NULL
        }
        get <- function() input
        setInverse <- function(inverse) invert <<- inverse
        getInverse <- function() invert
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(input, ...) {
    invert <- input$getInverse()
    if (!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    matrix <- input$get()
    invert <- solve(matrix, ...)
    input$setInverse(invert)
    invert
}
