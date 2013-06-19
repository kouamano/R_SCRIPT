
#正解データがついたデータを受けとって，正解ついたまま各クラスタごとに分割
#[[1]]で各クラスタのデータフレームにアクセス
split_by_result <- function(data){
		n <- ncol(data)
        each_cluster <- split(data, data[n])
        each_cluster
}

#listdataを入れてkの値を求める
k_num <- function(listdata){
	k <- length(listdata)
	k
}


#正解つきデータを受け取って，全データの中心をもとめる
#numericを返す
Ztot <- function(data){
	n <- ncol(data)
	cuted <- data[-n]
	me <- colMeans(cuted)
	me
}

#split_by_resultの結果を受け取って，各クラスタごとの中心を返す
#data.frameを返す，[n,]でクラスタを指定
Zk <- function(listdata){
	X <- data.frame()
	for(i in listdata){
		X <- rbind(X, Ztot(i))
	}
	X
}

#split_by_resultの結果を受け取って，各クラスタごとの要素数を返す
#data.frameを返す，[n,]でクラスタを指定
nk <- function(listdata){
	X <- data.frame()
	for(i in listdata){
		X <- rbind(X, nrow(i))
	}
	X
}

#Zk,Ztotの結果を受け取って，各クラスタ重心と全体重心の距離を返す
#matrixを返すk[n]で指定
dk <- function(zk_result, ztot_result){
	X <- rbind(zk_result, ztot_result)
	Y <- as.matrix(dist(X))
	n<- nrow(Y)
	Y <- Y[n,-n]
	Y
}

#Zk,nk,splitの結果を受け取って，各rkの結果を返す
#matrixを返す，k[n]で指定
rk <-function(zk_result, nk_result, listdata){
	x <- 1
	zk_result <- as.matrix(zk_result)
	all <- matrix()

	for(n in listdata){
		n <- as.matrix(n)
		nc <- ncol(n)
		rc <- n[,-nc]
		bn <- rbind(rc, zk_result[x,])
		Y <- as.matrix(dist(bn))
		m<- nrow(Y)
		Y <- Y[m,-m]
		Y <- sum(Y)
		Z <- Y / nk_result[x,]
		all <- rbind(all, Z)
		x <- x+1
	}
	all[-1]
}

#Ztotと，データを入れるとサンプルの平均半径
#数値が帰ってくる
Ra <- function(ztot_result, data){
	n <- nrow(data)
	i <- as.matrix(data)
	ii <- ncol(i)
	ri <- i[,-ii]
	bn <- rbind(ri, ztot_result)
	Y <- as.matrix(dist(bn))
	m<- nrow(Y)
	Y <- Y[m,-m]
	Y <- sum(Y)
	Y <- Y / n
	Y
}

#データを入れると次元を返す
dim <- function(data){
	d <- ncol(data) -1
	d
}

#データを入れるとデータ数を返す
all_n <- function(data){
	d <- nrow(data)
	d
}

#WCD用
#matrixを返す，k[n]で指定
r2k <-function(zk_result, nk_result, listdata){
	x <- 1
	zk_result <- as.matrix(zk_result)
	all <- matrix()

	for(n in listdata){
		n <- as.matrix(n)
		nc <- ncol(n)
		rc <- n[,-nc]
		bn <- rbind(rc, zk_result[x,])
		Y <- as.matrix(dist(bn))
		m<- nrow(Y)
		Y <- Y[m,-m]
		Y <- Y^2

		Y <- sum(Y)
		Z <- Y / nk_result[x,]
		Z <- sqrt(Z)

		all <- rbind(all, Z)
		x <- x+1
	}
	all[-1]
}


bcd <- function(dk_result,nk_result,all_n_result,k_num_result){
	all <- 0
	for(i in 1:k_num_result){
		x <- dk_result[i]^2 * nk_result[i,]
		all <- all + x
	}
	n_k <- all_n_result * k_num_result
	res <- all / n_k
	res
}

wcd <- function(r2k_result, k_num_result){
	s <- sum(r2k_result)
	s2 <- s / k_num_result
	s2
}

SF <- function(data){
	split_r <- split_by_result(data)
	zk_r <- Zk(split_r)
	knum_r <- k_num(split_r)
	ztot_r <- Ztot(data)
	nk_r <- nk(split_r)
	r2k_r <- r2k(zk_r, nk_r, split_r)
	alln_r <- all_n(data)

	dk_r <- dk(zk_r, ztot_r)

	wcd_r <- wcd(r2k_r, knum_r)
	bcd_r <- bcd(dk_r,nk_r,alln_r,knum_r)

    ss <- exp(1)^exp(1)^(bcd_r - wcd_r)
    s <- 1 - (1/ss)
    s
}


###################################################
wcsd <- function(data){
	split_r <- split_by_result(data)
	zk_r <- Zk(split_r)
	knum_r <- k_num(split_r)
	ztot_r <- Ztot(data)
	nk_r <- nk(split_r)
	alln_r <- all_n(data)
	dim_r <- dim(data)

	rk_r<- rk(zk_r, nk_r, split_r)
	dk_r <- dk(zk_r, ztot_r)
	Ra_r <- Ra(ztot_r, data)


	all <- 0
	for(i in 1:knum_r){
		m <- -((rk_r[i] / Ra_r)^dim_r)
		mm <- nk_r[i,]^m
		m3 <- (dk_r[i] * mm) / Ra_r
		all <- all + m3
	}
	k2 <- knum_r^2
	r_r <- all / k2
	r_r
}


##################################################

wcsd_m <- function(data,p,dim_r){
	split_r <- split_by_result(data)
	zk_r <- Zk(split_r)
	knum_r <- k_num(split_r)
	ztot_r <- Ztot(data)
	nk_r <- nk(split_r)
	alln_r <- all_n(data)

	rk_r<- rk(zk_r, nk_r, split_r)
	dk_r <- dk(zk_r, ztot_r)
	Ra_r <- Ra(ztot_r, data)


	all <- 0
	for(i in 1:knum_r){
		m <- -((rk_r[i] / Ra_r)^dim_r)
		mm <- nk_r[i,]^m
		m3 <- (dk_r[i] * mm) / Ra_r
		all <- all + m3
	}
	k2 <- knum_r^p
	r_r <- all / k2
	r_r
}

##################################################

wcsd_po <- function(data, p, dim_r){
	split_r <- split_by_result(data)
	zk_r <- Zk(split_r)
	knum_r <- k_num(split_r)
	ztot_r <- Ztot(data)
	nk_r <- nk(split_r)
	alln_r <- all_n(data)

	rk_r<- rk(zk_r, nk_r, split_r)
	dk_r <- dk(zk_r, ztot_r)
	Ra_r <- Ra(ztot_r, data)


	all <- 0
	for(i in 1:knum_r){
		m <- (Ra_r / rk_r[i] )^dim_r
		mm <- nk_r[i,]^m
		m3 <- (dk_r[i] * mm) / Ra_r
		all <- all + m3
	}
	k2 <- knum_r^p
	r_r <- all / k2
	r_r
}


##################################################
dir_index <- function(){
	tab <- list.files()
	

#####ここから組む，ファイル名を得られるので，その後indexを適用．これが完成したらしたの関数は消す


	tab_num <- length(tab)

	all <- data.frame(SF=0, WCSD=0)

	for(i in 1:tab_num){
		sf_r <- SF(tab[[i]])
		wcsd_r <- wcsd(tab[[i]])
		all <- rbind(all, c(sf_r, wcsd_r))
	}
	all
}



##################################################
dir_index_s <- function(){
	tab <- lapply(list.files(pattern="tsv"), read.table)
	tab_num <- length(tab)

	all <- data.frame(SF=0, WCSD=0)

	for(i in 1:tab_num){
		sf_r <- SF(tab[[i]])
		wcsd_r <- wcsd(tab[[i]])
		all <- rbind(all, c(sf_r, wcsd_r))
	}
	all
}



