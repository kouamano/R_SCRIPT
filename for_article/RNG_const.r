
####################################################
## RNG const
####################################################

rng_const<-function(data){
	n <- nrow(data)
	dis_data <- as.matrix(dist(data))
	y <- n-1

	RNG <- c(0,0,0)

	for (i in 1:y){
		x <- i+1
		for (j in x:n){
			flag <- 0
			for (k in 1:n){
				if((i==j) || (i==k) || (j==k)){
				}else if(dis_data[i,k] < dis_data[i,j]){
					if(dis_data[j,k] < dis_data[i,j]){
						flag <- 1
					}
				}
			}
			if(flag==0){
				RNG <- rbind(RNG, c(i,j,dis_data[i,j]))
			}
		}
	}
	RNG<- RNG[-1,]
	RNG
}


####################################################
## all path const
####################################################

#全部のペアをつくる
all_pair <- function(data){
	n <- nrow(data)
	y <- n-1

	pair <- list()

	for(i in 1:y){
		x <- i+1
		for (j in x:n){
			z <- c(i,j)
			pair <- c(pair, list(z))
		}
	}
	pair
}

#あるペアとグラフを受け取って，全経路を出力
pair_path <- function(pair ,graph){
	path_can <- list(pair[1])
	path_rec <- list()

	i <- 1
	loop <- 2
	while(i <= loop){
		www<- rev(path_can[[i]])[1]
		w <- which(graph==www, arr.ind=TRUE)
		w_num <- nrow(w)

		for(j in 1:w_num){
			if(w[j,2]==1){
				d <- graph[w[j,1], 2]
			}else if(w[j,2]==2){
				d <- graph[w[j,1], 1]
				
			}

			if(any(path_can[[i]] == d)){
				d <- d
			}else{
				z <- c(path_can[[i]], d)

				if(pair[2]==d){
					path_rec <- c(path_rec, list(z))					
				}else{
					path_can <- c(path_can, list(z))
				}
			}
		}
		loop <- length(path_can)
		i <- i+1
	}
	path_rec
}

#全部のペアを突っ込んで，全部の経路を得る
all_pair_path <- function(data){
	all_p <- all_pair(data)
	rng <- rng_const(data)
	list_path <- list()
	n <- length(all_p)

	for(i in 1:n){
		path <- pair_path(all_p[[i]], rng)
		list_path <- c(list_path, path)
	}

	list_path
}



####################################################
## maxmin
####################################################


#全部のペアを突っ込んで，全部の経路を得る
#maxminも計算する
all_pair_path_maxmin <- function(data){
	all_p <- all_pair(data)
	rng <- rng_const(data)
	list_path <- list()
	n <- length(all_p)

	for(i in 1:n){
		path <- pair_path(all_p[[i]], rng)
		m <- length(path)

		list_path_edge <- list()
		min_edge <- 0

		for(j in 1:m){
			max <- max_edge(path[[j]], rng)

			if(j == 1){
				min_edge <- max
				xxx <- list(path[[j]], min_edge)
				list_path_edge <- list(xxx)
			}else if(max < min_edge){
				min_edge <- max
				xxx <- list(path[[j]], min_edge)
				list_path_edge <- list(xxx)
			}
		}
		list_path <- c(list_path, list_path_edge)
	}
	list_path
}

#あるパスの最大のエッジ長
max_edge <- function(path, rng){
	max_edge <- 0
	m <- length(path)
	m <- m-1
	n <- nrow(rng)

	for(j in 1:m){
		a1 <- path[j]
		a2 <- path[j+1]

		if(a1 > a2){
			ppp <- a1
			a1 <- a2
			a2 <- ppp
		}

		for(i in 1:n){
			if(rng[i,1] == a1){
				if(rng[i,2] == a2){
					if(max_edge < rng[i,3]){
							max_edge <- rng[i,3]
					}
				}
			}
		}
	}
	max_edge
}

####################################################
## dshort
####################################################

d_short <- function(data){
	n <- nrow(data)
	maxmin <- all_pair_path_maxmin(data)
	len <- length(maxmin)

	mt <- matrix(0, n, n)
	x <- 1
	n1 <- n-1

	for(i in 1:n1){
		k <- i + 1
		for(j in k:n){
			mt[i,j] <- maxmin[[x]][[2]]
			x <- x+1
		}
	}
	
	for(s in 1:n){
		for(t in 1:n){
			mt[t,s] <- mt[s,t]
		}
	}
	mt
}

