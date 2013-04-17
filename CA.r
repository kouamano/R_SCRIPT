ca_clNsumples <- function(listed){
}

CA1 <- function(clinf){
	N <- clinf[[1]];
	R <- clinf[[2]];
	Cn <- clinf[[3]];
	rn <- clinf[[4]];
	rn = rn/R;
	list(Cn,
	rn,
	Cn^rn,
	prod(Cn^rn),
	prod(Cn^rn)*N)
}
