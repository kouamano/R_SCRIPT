
source("./RNG_const.r")

#正解データがついたデータを受けとって，正解ついたまま各クラスタごとに分割
#[[1]]で各クラスタのデータフレームにアクセス
split_by_result <- function(data){
		n <- ncol(data)
        each_cluster <- split(data, data[,n], drop=TRUE)
        each_cluster
}

db_s <- function(data,dshort){
	sp <- split_by_result(data)
	k_num <- length(sp)
	s_i <- c()

	for(k in 1:k_num){
		medoid <- ####medoid(k)	ひとつの数字が帰ってくる####

		data_num <- nrow(data)
		data_dim <- ncol(data)

		ck_num <- c()
		count <- 0

		for(i in 1:data_num){
			xxx <- data[i,data_dim]
			if(xxx == k){
				ck_num <- c(ck_num, i)
				count <- coundt + 1
			}
		}
	
		x<-0
		for(j in 2:count){
			ck <- ck_num[j]
			x <- x + dshort[medoid][ck]
		}

		z <- x / count
		s_i <- c(s_i, z)
	}
	s_i
}

R_i <- function(dshort,s_i,i){
	m <- length(s_i)
	max_num <- 0

	for(j in 1:m){
		if(i==j){
			i <- i
		}else{
			s1 <- s_i[i]
			s2 <- s_i[j]
			d1 <- dshort[i][j]
			x <- (s1 + s2) / d1

			if(i == 1){
				max_num <- x
			}else if(max_num < x){
				max_num <- x
			}

		}
	}
	max_num
}

connect_DB <- function(data){
	dn <- ncol(data)
	dd <- data[, -dn]

	d_mx <- d_short(dd)
	d_s_i <- db_s(data, d_mx)
	k <- length(d_s_i)

	x <- 0
	for(i in 1:k){
		y <- R_i(d_mx, d_s_i, i)
		x <- x + y
	}
	DB <- x / k
	DB
}


########################################################
########################################################
########################################################
########################################################
########################################################

delta <- function(data, dshort){
	sp <- split_by_result(data)
	k_num <- length(sp)
	max_num <- c()
	
	data_num <- nrow(data)
	data_dim <- ncol(data)
	dd1 <- data_num -1

	for(i in 1:k_num){
		mmm <- 0

		for(x in 1:dd1){
			x1 <- x+1
			for(y in x1:data_num){
				xxx <- data[x][data_dim]
				yyy <- data[y][data_dim]
				if((xxx == i)&&(yyy == i)){
					mm <- dshort[j][k]
					if(mmm < mm){
						mmm <- mm
					}
				}
			}
		}
		max_num <- c(max_num, mmm)

	}
	max_num
}

omega <- function(data, dshort){
	data_num <- nrow(data)
	data_dim <- ncol(data)

	sp <- split_by_result(data)
	k_num <- length(sp)
	mt <- matrix(0,data_num,data_num)

	dnum <- data_num -1

	for(i in 1:dnum){
		i1 <- i+1
		for(j in i1:data_num){
			d1 <- data[i][data_dim]
			d2 <- data[j][data_dim]
			
			if(d1==d2){
				d1 <- d2
			}else{
				ds1 <- dshort[i][j]
				dm1 <- mt[i][j]
				if(dm1 < ds1){
					m[i][j] <- ds1
				}
			}
		}
	}
	mt
}

connect_Dunn <- function(data){
	sp <- split_by_result(data)
	k_num <- length(sp)

	dn <- ncol(data)
	dd <- data[, -dn]
	dshort <- d_short(dd)

	k1 <- k_num -1
	bo <- delta(data, dshort)
	b <- max(bo)

	mt <- omega(data, dshort)

	min <- 9999999999999

	for(i in 1:k1){
		i1 <- i+1
		for(j in i1:k_num){
			m1 <- mt[i][j]
			if(min > m1){
				min <- m1
			}
		}
	}
	cD<- min / bo
	cD
}




