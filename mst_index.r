library("nnclust")

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

#正解つきデータを受け取り，尺度を返す
#mst1
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






