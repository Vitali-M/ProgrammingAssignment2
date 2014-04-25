############################
# makeCacheMatrix function #
############################


# Produces a list of 4 functions 

# set : gets the value of a matrix x, 
#            stores it in a global environment,
#            defines a NULL matrix m in a global environment ;

# get : retrieves the value of the x matrix ;

# setsolve : computes the inverse of x using solve function, 
#                 stores it as m in the global environment (= cached data),  

# getsolve : retrieves the stored value of m. 


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) {
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


############################
# cacheSolve function      #
############################


# Returns the inverse of a matrix from the list above 

# retrieves the value of the m inverse matrix from getsolve function
# if m not NULL ~:
#   prints message : "getting cached data"
#   returns m
# otherwise :
#   gets the value of x from get function
#   computes m using solve function
#   stores it in cached data using setsolve function
#   returns m

cacheSolve <- function(x, ...) {
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


###########
# Example #
###########

mat <- matrix(c(1,2,3,4),2,2, byrow=T)
x <- makeCacheMatrix(mat)

cacheSolve(x) # 1st call : computes inverse matrix and stores in cached data 
cacheSolve(x) # 2nd call : gets cached data from 1st call

