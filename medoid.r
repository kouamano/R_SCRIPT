medoidc <- function(mat){
	pos = 1;
	minv = 0;
	#print(nrow(mat));
	v = c();
	tmat = t(mat);
	for( i in 1:nrow(tmat) ){
		#print(i);
		#print(mean(tmat[i,]));
		v=cbind(v,mean(tmat[i,]));
	}
	#print(v);
	rmat=rbind(mat,v);
	#print(dist(rmat));
	targetList=as.matrix(dist(rmat))[nrow(mat)+1,-nrow(mat)-1];
	pos=which(targetList == min(targetList));
	#print(pos);
	return(list(pos,mat[pos,]));
};

medoidd <- function(mat){
	pos = 0;
	#minv = 0;
	#print(nrow(mat));
	#v = c();
	tmat = t(mat);
	#for( i in 1:nrow(tmat) ){
		#print(i);
		#print(mean(tmat[i,]));
		#v=cbind(v,mean(tmat[i,]));
	#}
	#print(v);
	#rmat=rbind(mat,v);
	#print(dist(rmat));
	#targetList=as.matrix(dist(rmat))[nrow(mat)+1,-nrow(mat)-1];
	targetList=as.matrix(dist(tmat));
	print(targetList);
	pos=which(targetList == min(targetList));
	#print(pos);
	#return(list(pos,mat[pos,]));
};
