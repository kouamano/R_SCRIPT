CA1 <- function(clinf){
	N <- clinf$value[[1]];
	R <- clinf$value[[2]];
	Cn <- clinf$value[[3]];
	rn <- clinf$value[[4]];
	rn = rn/R;
	list(Cn,
	rn,
	Cn^rn,
	prod(Cn^rn),
	prod(Cn^rn)*N)
}
