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
