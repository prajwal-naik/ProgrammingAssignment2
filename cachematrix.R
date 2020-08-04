#These function find the inverse of a matrix. 
#However, since the processing of inverting a matrix is not resource friendly, the functions tcashe thevalue of the inverse for future use

#This function creates a special "matrix" object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) 
{
        inv<-matrix()
        set<-function(y)
        {
                x<<-y
                inv<<-matrix()
        }
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#This function checks if the inverse exists in cache. Else it finds it's inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!all(is.na(inv)))
        {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv<-solve(data, ...)
        x$setinverse(inv)
        inv
}
