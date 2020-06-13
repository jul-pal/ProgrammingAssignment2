## This function is intended to cache the inverse of an invertible matrix. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
## More precisely,it builds a set of functions (set, get, setsolve, getsolve) alongside two objects x and inv.
## Subsequently, the functions are named and stored within a list to the parent environment alongside the objects, such that they 
## can be accessed in the cacheSolve function the via the $ extract operator. 


makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        ##Objects are initialized. 
        set<-function(y){
                x<<-y
                ##<<- assignes y to the to the object x that is located in the parent environment. 
                inv<<-NULL
        }
        get<-function() x
        ## Because x is defined outside of the get() function, lexical scoping implies that it is retrieved from the parent environment instead.
        setsolve<-function(solve) inv<<-solve
        ## <<- assigns the input argument (solve) of the set solve function to the value of inv in the parent environment. 
        getsolve<-function() inv
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}

##CacheSolve: This function computes the inverse of the special "matrix" x returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## The ellipis ... allows for additional arguments to ?be passed through the function. 
        inv<-x$getsolve()
        ## Retrieves the stored inverse, and if unequal to NULL returns it from the cache.
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        ## Retrieves the stored matrix 'x' and saves it as data.
        inv<-solve(data,...)
        ## Computes the inverse of the matrix and denotes it as inv.
        x$setsolve(inv)
        ## Sets the resulting inverse to be inv in the cache as specified by makeCacheMatrix, such that when the fuction is called again, 
        ## the if condition will apply, and the inverse will be returned form memory.
        inv
        ## Returns a matrix that is the inverse of 'x' (the argument of makeCacheMatrix)
}

