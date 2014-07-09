#d_shortを作成する
#input: RNG
#output: d_short
#ok
d_short <- function(rng){
	rng[,1] <- rng[,1]+1
	rng[,2] <- rng[,2]+1

	n <- ncount(rng)
	p <- rowcol(n)
	d_data <- dsc(n,p,rng)
	d_data
}

#個々のペア, rng, nを受け取りminmaxを返す
#OK
minmax <- function(pair, rng, n){
	start <- pair[1]
	goal <- pair[2]

	value_vec <- c(Inf)
	route_list <- list(start)
	maxmin_v <- 0
	flag <- 0

	repeat {
		f_index <- find_index(value_vec)
		num_index <- length(f_index)

		for(i in 1:num_index){
			now_index <- f_index[i]

			x <- length(route_list[[now_index]])
			if(goal == route_list[[now_index]][[x]]){
				maxmin_v <- value_vec[now_index]
				flag <- 1
				break
			}

			current_value <- value_vec[now_index]
			current_route <- route_list[[now_index]]
			long <- length(route_list[[now_index]])
			current_node <- route_list[[now_index]][[long]]

			nextnode <- find_next(current_route,rng)

			if(is.null(nextnode)){
			}else{
				loop <- length(nextnode)
				for(k in 1:loop){

					if(current_node > nextnode[k]){
						loc1 <- subset(rng, rng[,2]==nextnode[k])
						loc2 <- subset(loc1,loc1[,1]==current_node)
						n_long <- loc2[1,3]
					}else{
						loc1 <- subset(rng, rng[,2]==current_node)
						loc2 <- subset(loc1,loc1[,1]==nextnode[k])
						n_long <- loc2[1,3]
					}

					if(current_value == Inf){
						now_value <- n_long
					}else if(n_long > current_value){
						now_value <- n_long
					}else{
						now_value <- current_value
					}

					next_route <- append(current_route, nextnode[k])

					value_vec <- append(value_vec, now_value)
					route_list <- append(route_list, list(next_route))
				}
			}
		}
		if(flag == 1){
			break
		}else{
			r_num_index <- rev(f_index)
			for(i in 1:num_index){
				w_index <- r_num_index[i]
				value_vec <- value_vec[-w_index]
				route_list <- route_list[-w_index]
			}
		}
	}
	maxmin_v
}

#ルートとRNGを入れて次のノードを求める 次のノードたちか，NULL返す
#既に通過したノードは含まれていない
#OK
find_next <- function(cr,rng){
	l_num <- length(cr)
	now_node <- cr[l_num]
	rng_no_value <- rng[,-3]

	ind <- which(rng_no_value == now_node, arr.ind=TRUE)
	num <- nrow(ind)

	n_can <- c()

	for(i in 1:num){
		w <- ind[i,2]
		r <- ind[i,1]
		if(w == 1){
			c <- rng[r,2]
		}else if(w == 2){
			c <- rng[r,1]			
		}
		n_can <- append(n_can, c)
	}

	n_can_num <- length(n_can)
	next_node <- c()
	
	for(j in 1:n_can_num){
		nn <- any(cr == n_can[j])
		if(nn == TRUE){
		}else{
			next_node <- append(next_node, n_can[j])
		}
	}
	next_node
}

#vectorのminのindexの値をもとめる
#OK
find_index <- function(value_vec){
	i <- which(value_vec == min(value_vec))
	i
}

#d_shortを返す
#ok
dsc <- function(n,p,rng){
	d_data <- matrix(nrow=n,ncol=n)
	p_row <- nrow(p)
	for(i in 1:p_row){
		m_v <- minmax(p[i,],rng,n)
		d_data[p[i,1],p[i,2]] <- m_v
	}
	d_data
}

#求める行，列のペアの行列
#  0からノード番号がはじまるケースに対応していない
rowcol <- function(n){
	n0 <-(n*(n-1))/2
	pair <- matrix(nrow=n0,ncol=2)
	k <- 1
	n1 <- n - 1
	for(i in 1:n1){
		i1 <- i+1
		for(j in i1:n){
			pair[k,1] <- i
			pair[k,2] <- j
			k <- k+1
		}
	}
	pair
}

#dataのポイント数を数える
#ok
ncount <- function(rng){
	x <- rng[,1]
	y <- rng[,2]
	z <-c(x,y)
	xx <- unique(z)
	n <- length(xx)
	n
}


