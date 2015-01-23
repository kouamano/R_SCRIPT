library("nnclust")
library("cluster")

#正解データがついたデータを受けとって，正解ついたまま各クラスタごとに分割
#[[1]]で各クラスタのデータフレームにアクセス
split_by_result <- function(data){
		n <- ncol(data)
        each_cluster <- split(data, data[n])
        each_cluster
}

#mstを受け取りパスの平均の長さを返す
mst_path_mean <- function(mst){
	n <- length(mst$dist)
	mst_sum <- sum(mst$dist)
	mm <- mst_sum/n
	mm
}

#正解つきデータを受け取り，尺度を返す．
#クラスタ内のパスの平均長を出して，クラスタごとの平均長の平均をだす
#mst1:データ数＝クラスタ数のときもっともよい
mst1_index <-function(data){
	each_cluster <- split_by_result(data)
	knum <- length(each_cluster)
	nc <- ncol(data)

	all_sum <- 0

	for(i in 1:knum){
		md1 <- each_cluster[[i]]
		nr <- nrow(md1)
		if(nr==1){
		}else{
			md2 <- md1[,-nc]
			md3 <- as.matrix(md2)
			mst <- mst(md3)
			cluster_sum <- mst_path_mean(mst)
			all_sum <- all_sum + cluster_sum
		}
	}
	ans <- all_sum / knum
	ans
}

#クラスタ間距離をmst1に組み込む
#クラスタ間距離：各クラスタの重心近くの点でMSTを作成，平均を計算
#クラスタ内距離（mst1）/クラスタ間距離 小さいほどよい

mst2_index <-function(data){
	m1 <- mst1_index(data)
	b1 <- between_mst(data)

	an1 <- m1/b1
	an1
}

#メディアンだけでMSTをつくって，その平均
between_mst <-function(data){
	sp <- split_by_result(data)
	knum <- length(sp)
	nc <- ncol(data)
	nn <- nc - 1

	xyz <- matrix(nrow=knum, ncol=(nc-1))

	for(i in 1:knum){
		x <- center_xy(sp[[i]])
		for(j in 1:nn){
			xyz[i,j] <- x[j]
		}
	}

	#以下，メディアンでMSTをつくり平均をだす
	mst1 <- mst(xyz)
	all_mean <- mst_path_mean(mst1)
	all_mean
}

#クラスタに別れたデータを受け取る（答え付き）
#メディアンをベクトルで返す
center_xy <- function(data){
	md <- data
	nr <- nrow(data)
	nc <- ncol(data)

	md_no_ans <- md[,-nc]

	if(nr == 1){
		a1 <- as.vector(as.matrix(md_no_ans))
		a1
	}else{
		pm	<- pam(md_no_ans, 1)
		a1 <- as.vector(pm$medoids)
		a1
	}
}

#行列を受け取ってMSTパスの総距離を返す
mst_all_plus <- function(xyz){
	mst_xyz <- mst(xyz)
	s1 <- sum(mst_xyz$dist)
	s1
}

#クラスタ間距離をmst1に組み込む 二個目（ペアワイズでBetween）
#クラスタ間距離：クラスタをペアでくっつけてMST作って，総距離
#クラスタ内距離（mst1）/クラスタ間距離 小さいほどよい
mst3_index <- function(data){
	m1 <- mst1_index(data)
	b1 <- pair_between_mst(data)
	an1 <- m1/b1
	an1

}

#クラスタをペアでくっつけてMSTをつくり，エッジの平均をとる
#さらに求めた平均を全てたして，ペアの数で割って平均をとった値
pair_between_mst <- function(data){
	sp <- split_by_result(data)
	knum <- length(sp)
	nc <- ncol(data)

	k <- knum - 1
	all_path <- 0
	n <- 0

	for (i in 1:k){
		l <- i + 1
		for (j in l:knum){
			sdata <- rbind(sp[[i]], sp[[j]])
			sdata_no_ans <- sdata[,-nc]
			p1 <- mst(as.matrix(sdata_no_ans))
			all_path <- mst_path_mean(p1) + all_path
			n <- n + 1
		}
	}
	a1 <- all_path / n
	a1
}



#正解つきデータを受け取り，尺度を返す．
#クラスタ内のパスの平均長を出して，たす
#mst1=>4:データ数＝クラスタ数のときもっともよい
mst4_index <-function(data){
	each_cluster <- split_by_result(data)
	knum <- length(each_cluster)
	nc <- ncol(data)

	all_sum <- 0

	for(i in 1:knum){
		md1 <- each_cluster[[i]]
		nr <- nrow(md1)
		if(nr==1){
		}else{
			md2 <- md1[,-nc]
			md3 <- as.matrix(md2)
			mst <- mst(md3)
			cluster_sum <- mst_path_mean(mst)
			all_sum <- all_sum + cluster_sum
		}
	}
	ans <- all_sum
	ans
}


#クラスタ間距離をmst4に組み込む
#クラスタ間距離：各クラスタの重心近くの点でMSTを作成，平均を計算
#クラスタ内距離（mst4）/クラスタ間距離 小さいほどよい

mst5_index <-function(data){
	m1 <- mst4_index(data)
	b1 <- between_mst(data)

	an1 <- m1/b1
	an1
}

#クラスタ間距離をmst4に組み込む
#クラスタ間距離：各クラスタの重心近くの点でMSTを作成，平均を計算
#クラスタ内距離（mst4）/クラスタ間距離(+) 小さいほどよい

mst6_index <-function(data){
	m1 <- mst4_index(data)
	b1 <- between_mst6(data)

	an1 <- m1/b1
	an1
}

#メディアンだけでMSTをつくって，+
between_mst6 <-function(data){
	sp <- split_by_result(data)
	knum <- length(sp)
	nc <- ncol(data)
	nn <- nc - 1

	xyz <- matrix(nrow=knum, ncol=(nc-1))

	for(i in 1:knum){
		x <- center_xy(sp[[i]])
		for(j in 1:nn){
			xyz[i,j] <- x[j]
		}
	}

	#以下，メディアンでMSTをつくり+
	mst1 <- mst(xyz)
	all_mean <- mst_path_mean(mst1)
	v1<-all_mean*knum
	v1
}


#クラスタ内距離:クラスタごとにMST作って平均
#クラスタ間距離:クラスタの違うデータ間の全ての距離を用いる，クラスタの組み合わせごとに平均を作って平均
#内距離を間距離で割る．小さいほうがよい
mst7_index <-function(data){
	m1 <- mst1_index(data)
	b1 <- between_pont_distance(data)

	an1 <- m1/b1
	an1
}

#クラスタ間距離:クラスタの違うデータ間の全ての距離を用いる，クラスタの組み合わせごとに平均を作って平均
#答えは1始まり
between_pont_distance <- function(data){
	nc <- ncol(data)
	ans_max <- data[which.max(data[,nc]),nc]
	ans_min <- data[which.min(data[,nc]),nc]

	x <- list()
	for(i in ans_min:ans_max){
		y <- which(data[,nc]==i)
		x[[i]] <- y
		i <- i + 1
	}

	dm_data <- as.matrix(dist(data))

	ans_max_m1 <- ans_max - 1
	all_sum <- 0
	count <- 0

	for(i in ans_min:ans_max_m1){
		k <- ans_min + 1
		for(j in k:ans_max){
			xx <- cluster_dis_mean(x[[i]], x[[j]], dm_data)
			all_sum <- all_sum + xx
			count <- count + 1
		}
	}
	resu <- all_sum / count
	resu
}

#答えのリストから2クラスタ分，as.matrix（dist）されたデータを受け取る
#2つのクラスタ間の距離の平均を返す
cluster_dis_mean <- function(list1, list2, dm_data){
	num_c1 <- length(list1)
	num_c2 <- length(list2)

	allsum <- 0
	comb <- num_c1 * num_c2
	for(i in list1){
		for(j in list2){
			allsum <- allsum + dm_data[i, j]
		}
	}

	x <- allsum / comb
	x
}






#受け取ったデータを最終列でソートして返す
data_sort <- function(data){
	nc <- ncol(data)
	data_order <- order(data[,nc])
	a_data <- data[data_order,]
	a_data
}

#受け取ったデータをソートした上でのdistを返す
sort_dis <- function(data){
	nc <- ncol(data)
	s_data <- data_sort(data)
	di <-dist(s_data[,-nc])
	di
}

