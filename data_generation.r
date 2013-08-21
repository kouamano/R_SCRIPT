randCircle2D <- function(n,rmin,rmax){
	d <- array(c(runif(n,min=rmin,max=rmax),runif(n,min=-1.1*pi,max=1.1*pi)),c(n,2))
	t(mapply(function(x,y) polarToXY(x,y),d[,1],d[,2]))
}
