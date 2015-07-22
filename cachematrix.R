## Put comments here that give an overall description of what your
## functions do

## Essentially, we are storing 2 values here. After NULLing one of them (which will be the inverse of the matrix), 
## a function to locate the value of the argument is created, later a function to output this value. 
## Then that is "repeated" for the value that we made NULL in the first line, which will be calculated with cachesolve.



makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set<-function(y){
        		y<<-x
        		inv<<-NULL        	
        }
        get <- function() x
        setinv <- function(Y) Y<<-inv
        getinv <- function() inv
        list(inv = inv, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Here we are calling some of the created functions (or getting the cached data if there are), 
## do you have "inv" value?? ok, you dont? ok let's make the operation, which I cannot remember exactly now, 
## it is like submatrix multiplication covering the opposite possitions divided by the its determinant right? 
## Thanks we have machines remembering that for us. LOVELY !!

cacheSolve <- function(x, ...) {
	    inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
        ## Just a point: I do not really understand the <<- operator: when doing x<-10 we are giving x the value of 10.
        ## When doing Y<<-inv, I am giving inv the value of Y (otherwise Y would be NULL). 
        ## It's a little of confusing...
}
