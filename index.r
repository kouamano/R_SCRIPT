#正解データがついたデータを受けとって，正解ついたまま各クラスタごとに分割
#[[1]]で各クラスタのデータフレームにアクセス
split_by_result <- function(data){
		n <- ncol(data)
        each_cluster <- split(data, data[n])
        each_cluster
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















