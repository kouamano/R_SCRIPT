addresult <- function(data, km, k){
        result_data <- cbind(data, km$cluster)
        result_list <- list(result_data)
        each_cluster <- split(result_data, km$cluster)
        listed <- list(result_list, each_cluster)
        listed
}

all_R <- function(data){
	km1 <- kmeans(data, 1)
	sample_num <- nrow(data)
	co <- 0
	
	for (i in 1:sample_num){
		bind_center <- rbind(data[i,], km1$centers)
        d_center <- dist(bind_center)
        co <- co + d_center[1]
    }
	aR <- co / sample_num
	aR
}

cluster_r <- function(kcenter,listed,k_number){
	count <- 0
	num1 <- nrow(listed[[2]][[k_number]])

	for(i in 1:num1){
		row_1 <- listed[[2]][[k_number]][i,]
        len <- length(row_1)
        cutlast <- row_1[, -len[1]]
        cluc <- rbind(kcenter, cutlast)
        dis <- dist(cluc)
		count <- count + dis[1]
	}
	clr <- count / num1
	clr
}

single_cl <- function(data,kcenter,num,addlist,allr,allsamplenum,s){
	clr <- cluster_r(kcenter, addlist, num)
	single <- (clr / allr +1) / (s / allsamplenum)
	single
}


Psingle_cl <- function(data,kcenter,num,addlist,allr,allsamplenum,s){
	clr <- cluster_r(kcenter, addlist, num)
	psin <- s^(-(clr/allr))
	psin
}


sc <- function(data,k){
	km <- kmeans(data,k)
	addr <- addresult(data, km, k)
	allr <- all_R(data)
	allsam <- nrow(data)


	ss <- 0
	for(i in 1:k){
		sk <- nrow(addr[[2]][[i]])
		sing <- single_cl(data,km$centers[i,],i,addr,allr,allsam,sk)
		ss <- ss + sing
	}
	ss
}

psc <- function(data,k){
	km <- kmeans(data,k)
	addr <- addresult(data, km, k)
	allr <- all_R(data)
	allsam <- nrow(data)

	pss <- 1
	for(i in 1:k){
		sk <- nrow(addr[[2]][[i]])
		psing <- Psingle_cl(data,km$centers[i,],i,addr,allr,allsam,sk)
		pss <- pss * psing
	}
	pss
}



