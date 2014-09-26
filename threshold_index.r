#正解データがついたデータを受けとって，正解ついたまま各クラスタごとに分割
#[[1]]で各クラスタのデータフレームにアクセス
split_by_result <- function(data){
		n <- ncol(data)
        each_cluster <- split(data, data[n])
        each_cluster
}

#答え付なしのデータ，スレッショルドを受け取って，データの距離行列からスレッショルド未満の部分をNAにした距離行列を返す
cut_by_threshold <- function(data, threshold){
	dist_data <- as.matrix(dist(data))
	out_data <- which(dist_data > threshold)
	dist_data[out_data] <- NA
	dist_data
}

#各データポイントごとに，同一クラスタ内の他のデータポイントへの距離が入った１行のベクトルを受け取る．距離がないときはNA
#平均の逆数をだして出力
#　問題？：同一の点への距離はどうする？　0だと問題あり
#現状だと0扱い
density_each_datapoint <-function(line){
	l <- length(line)
	m <- l / sum(line, na.rm=TRUE)
	m
}

#各クラスタのdist(スレッショルド済み)を受け取り，クラスタ内のデータポイントの密度の平均を返す
density_each_cluster <- function(cb_data){
	nr <- nrow(cb_data)
	s <- 0
	for(i in 1:nr){
		each_d <- density_each_datapoint(cb_data[i,])
		s <- s + each_d
	}
	m <- s / nr
	m
}

#正解つきデータとスレッショルドを受け取る
#個々のデータごとに密度を出す=>クラスタごとに平均を出す
#全クラスタで平均を出す
#大きい値ほどよい
threshold_index1 <- function(data, threshold){
	nc <- ncol(data)
	each_cluster <- split_by_result(data)
	knum <- length(each_cluster)

	sum_cluster_denity <- 0

	for(i in 1:knum){
		ec_d <- each_cluster[[i]]
		cb_data <- cut_by_threshold(data[,-nc], threshold)
		density_c <- density_each_cluster(cb_data)
		sum_cluster_denity <- sum_cluster_denity + density_c
	}

	ans1 <- sum_cluster_denity / knum
	ans1
}

#スレッショルド=平均の場合
threshold_index1_mean <- function(data){
	nc <- ncol(data)
	no_ans_data <- data[,-nc]
	d_data <- dist(no_ans_data)
	s1 <- sum(d_data)
	l1 <- length(d_data)
	th <- s1/l1

	a0 <- threshold_index1(data, th)
	a0
}




direct_allfile_app_index_t <- function(p=2){
	file_name <- list.files()

	data_with_ans <- lapply(list.files(pattern="tsv"), read.table)
	data_num <- length(data_with_ans)

	all <- data.frame(TH1=0, TH2=0)

	for(i in 1:data_num){
		th1_i <- threshold_index1_mean(data_with_ans[[i]])
		th2_i <- 0
		all <- rbind(all, c(th1_i, th2_i))
	}

	all <- all[-1,]
	cbind(file_name,all)
}






