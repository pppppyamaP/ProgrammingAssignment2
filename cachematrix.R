# 関数 1: makeCacheMatrix (テンプレート)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# 関数 2: cacheSolve (テンプレート)
cacheSolve <- function(x, ...) { # 'x' は makeCacheMatrix が返したリスト
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat_data <- x$get()
    inv <- solve(mat_data, ...) # solve() で逆行列を計算
    x$setInverse(inv)
    inv
}
