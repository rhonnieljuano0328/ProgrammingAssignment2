makeCachematrix <- function(a = matrix()) {
  MEQ <- NULL
  set <- function(y) {
    a <<- y
    MEQ <<- NULL
  }
  get <- function() {a}
  setInverse <- function(inverse) {MEQ <<- inverse}
  getInverse <- function() {MEQ}
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

sourcacheSolve <- function(a, ...){
  MEQ <- a$getInverse()
  if(!is.null(MEQ)){
    message("getting the cached data")
    return(MEQ)
  }
  data <- a$get()
  MEQ <- solve(data, ...)
  a$setInverse(MEQ)
  MEQ
}
