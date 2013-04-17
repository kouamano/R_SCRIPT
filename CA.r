source("sf.r")

CA1 <- function(N,R,Cn,rn){
	rn = rn/R;
	list(Cn,
	rn,
	Cn^rn,
	prod(Cn^rn),
	prod(Cn^rn)*N)
}
