rm(list=ls())
### Caching the results of costly computations

# Caching is preferred to saving as a new variable because the functions below check if the data is unchanged and call the cached value
# Useful when we do not know when the data had changed


# makeVector returns a list
# Each element of the list is a function
# You can reference elements of a list by name like x$get.


makeVector <- function(x = numeric()) { 
    m <- NULL

    # $set() should invalidate the cache and set the elements of the vector to the new values, y. 
    set <- function(y) {                     # The set() function allows us to change the values and update the mean estimate
        x <<- y                              # Set the value of the vector x to y (y is specified outside of makeVector)
        m <<- NULL                           # Resets the mean to NULL
    }
    
    get <- function() x                      # Returns the vector x
    setmean <- function(mean) m <<- mean     # Specifies use of mean() function?
    getmean <- function() m                  # Returns m, the mean
    
    list(set = set, get = get,               # Returns vector of functions
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()                        # Pulls the value of m from the cache
    if(!is.null(m)) {                       # Checks to see if m has already been calculated
        message("getting cached data")
        return(m)                           # If the -if- statement is valid and return() is called, this stops the function
    }
    data <- x$get()                         # If mean is not in cache, stores data in x in "data" 
    m <- mean(data, ...)                    # Calculates the mean
    x$setmean(m)                            # Stores the mean in cache
    m                                       # Prints the mean; can use return(m) as well
}

# Using the functions above
vec <- makeVector(x=1:10)                   # Assign the makeVector list of functions with x defined 
cachemean(vec)
ls(environment(vec$get))                    # Note the "x" field


# The <<- operator:
# Normally the scope of a variable is the function that it is defined in. 
# When you use the <<- operator, it looks outside the current scope starting with the parents environment.
# environment() - lists the current environments
# Generally, <<- should only be used with lexical scoping
# if you define a function inside another function, you can use <<- to modify the variables defined in the parent environment from the child function's local environment. 
# This type of child functions are called closures because they enclose the environment of the parent function and can access all its variables. 
# This is useful because it allows control of variables in two different levels: a parent level that controls operation and a child level that does the work. 
# Having variables at two levels allows you to maintain state across function invocations.
# This is possible because while the execution environment is refreshed every time, the enclosing environment is constant. 


# Illustrating the role of the set() function within makeVector
# If set is defined in the definition of makeVector(), then x will be in that scope.

z <- makeVector(c(1,2,3))
z$get()
cachemean(z)
cachemean(z)                                 # Pulls the mean from the cache
z$set(c(4,5,6))                              # Change the values in z using the set() function
z$get()                        
cachemean(z)                                 # Note different mean value; it is stored in cache in place of the original
cachemean(z)                                 # Pulls the new mean from the cache



# Getting the inverse of a matrix
mat <- matrix(c(1,2,3,0,1,4,5,6,0), ncol=3, nrow=3)
mat
matInv <- solve(mat)




