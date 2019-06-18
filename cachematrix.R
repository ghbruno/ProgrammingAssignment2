## Both functions are very similar to those in the cachemean.R example.

## The first one (makeCacheMatrix) basically stores the input matrix - the one that is going to be inverted -
## and creates the 'sets and gets' functions that will be used in the second function (cacheSolve).

## The second function (cacheSolve) computes and stores in the cache the inverted matrix, based on 
## the input matrix of the fist function (makeCacheMatrix).

## The best way to use these function is:
## 1) Assign the desired input matrix to a variable.
##      ex: u <- matrix(c(1,-3,4,6),2,2)
## 2) Run the first function using the 'u' variable as the argument and assing it to another variable. 
##      ex: SomeMatrix <- makeCacheMatrix(u)
## 3) Run the second function using the 'SomeMatrix' variable as the argument.
##      ex: cacheSolve(SomeMatrix)

## The result will be the inverted matrix of the 'u' matrix. If the inverted matrix of the 'u' matrix have 
## already been computed the stored result in the cache will be returned.
## If not, the funcion will compute and stored the result in the cache. 

## The function below does the following:
## 1) nests the set function, that sets the input matrix into the 'x' variable. 
## 2) stores the input matrix 'x' (which is the argument of the function) in the nested get function.
## 3) nests the set_inverted_matrix function which sets the result of the cacheSolve function into the 'm' variable.
## 4) nests the get_inverted_matrix function which returns the inverted_matrix ('m' variable)
## 5) creates a list which each element is one of the nested functions above.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() {
                x
        }
        
        set_inverted_matrix <- function(inverted_matrix) {
                m <<- inverted_matrix
        }
        
        get_inverted_matrix <- function() {
                m       
        }
        
        list(set = set, get = get, set_inverted_matrix = set_inverted_matrix, get_inverted_matrix = get_inverted_matrix)

}

## The function below does the following:
## 1) assigns to the 'm' variable the inverted_matrix stored in cache. 
## 2) returns the inverted  matrix cached ('m' variable) alongside a message if its not null and ends the function.
## If there isn't a cached inverted matrix then:
## 3) gets the input matrix stored in the cache and assings it to the 'data' variable.
## 4) invert the input matrix ('data' variable') through the solve function and assigns it to the 'm' variable,
## storing it in the cache.
## 5) auto print the inverted matrix ('m' variable).

cacheSolve <- function(x, ...) {

        m <- x$get_inverted_matrix()
        
        if (!is.null(m)) {
                message("getting cached inverted matrix")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$set_inverted_matrix(m)
        m
}
