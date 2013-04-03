Gini.index <- function( y,main="",xlab="",ylab=""){
	        stopifnot(y >= 0)     
        n <- length(y)             
        y <- sort(y)            
        y <- cumsum(y)       
        y <- c(0, y/y[n])   
        x <- seq(0, 1, length=n+1)   
        plot(x, y, type="l", col="blue",   
                main=main, xlab=xlab, ylab=ylab)
        abline(v=c(0, 1), h=c(0, 1),     
                coef=c(0, 1))          
        return(2*sum(x-y)/n)        
}
