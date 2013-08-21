polarToXY <- function(r,t){
	c(r*cos(t),r*sin(t))
}

pXYToPolar <- function(x,y){
	c(sqrt(x^2+y^2),arccos(x/sqrt(x^2+y^2)))
}

nXYToPolar <- function(x,y){
	c(sqrt(x^2+y^2),pi*2 - arccos(x/sqrt(x^2+y^2)))
}

XYToPolar <- function(x,y){
	if(y>=0){
		pXYToPolar(x,y)
	}else{
		nXYToPolar(x,y)
	}
}

randCircle2D <- function(n,rmin,rmax){
        d <- array(c(runif(n,min=rmin,max=rmax),runif(n,min=-1.1*pi,max=1.1*pi)),c(n,2))
        t(mapply(function(x,y) polarToXY(x,y),d[,1],d[,2]))
}

