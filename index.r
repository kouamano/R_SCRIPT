split_by_result <- function(data){
		n <- ncol(data)
        each_cluster <- split(data, data[n])
        each_cluster
}
#[[1]]でデータフレームにアクセス

Ztot <- function(data){
	n <- ncol(data)
	cuted <- data[-d]
	me <- colMeans(cuted)
	me
}






Zi <- function(listx){
	X <- list()
	for(i in listx) X <- append(X, Ztot(i))
	X
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

dkR <- function(dk, r){
	result <- dk /r
	result
}

Sk <- function(sk, rk, r, dim){
	ni <- rk/r
	iti <-  -(ni^dim)
	result <- sk^iti

#	kakko <- rk / r
#	bunbo <- kakko^dim
#	result <- sk / bunbo
	result
}

dk <- function(km,km1,n){
	kk <- km$centers[n,]
	k1 <- km1$centers[1,]
	re <- rbind(kk,k1)
	d <- dist(re)
	result <- d[1]
	result
}


siguma <- function(dk, r, sk, rk, dim){
	mae <- dkR(dk, r)
	ato <- Sk(sk, rk, r, dim)
	result <- mae * ato
	result
}

a_index <- function(data,k,km1){
	km <- kmeans(data,k)
	addr <- addresult(data, km, k)

	dim <- ncol(data)
	r <- all_R(data)

	result <- 0
	for(i in 1:k){
		sk <- nrow(addr[[2]][[i]])
		dk <- dk(km,km1,i)
		rk <- cluster_r(km$centers[i,], addr ,i)

		sig <- siguma(dk, r, sk, rk, dim)
		result <- sig + result
	}
	kbun <- result / k
	kbun
}

amano_index <- function(data,k){
	k1 <- kmeans(data,1)
	result <- a_index(data, k, k1)
	result
}

amano_index_loop <- function(inputdata,loop){
	a_loop <- list()
#	datanum <- nrow(inputdata) -1
	datanum <- 10

	k1 <- kmeans(inputdata,1)

	for(m in 1:loop){
		a_list <- list()

		for(n in 1:datanum){
			a_result <- a_index(inputdata,n,k1)
			a_list[length(a_list)+1] <- a_result
		}
		
		if(m == 1){
			a_loop <- a_list
		}else{
			for (y in 1:length(a_list)){
				if(a_loop[[y]] < a_list[[y]]){
					a_loop[[y]] <- a_list[[y]]
				}
			}
		}
	}
	a_loop
}





