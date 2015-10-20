## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL # inversion of x
      
      set <- function(y) {   #set function
	  x <<- y
	  inv <<- NULL 
      }

      get <- function() x  # get function
      setInv <- function(inversa) inv <<- inversa  # setto il valore della inversa
      getInv <- function() inv # fornisco il valore della inversa
      
      list(set = set, get = get,  # creo lista con le 4 funzioni
	       setInv = setInv,
	       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInv() # get the inversed x matrix 
      if(!is.null(m)) {   # if m not null, take the cached data
	  message("getting cached data")
	  return(m) 
      }
      data <- x$get()      #  funzione get di x
      m <- solve(data)     #  calcolol'inversa di data
      x$setInv(m)          #  chiamo la funzione set di x
      m		 	   #  resrituisco l'inversa
}
