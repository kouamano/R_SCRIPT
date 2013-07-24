
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

#split_by_resultの結果を受け取って，各クラスタごとの要素数を返す
#data.frameを返す，[n,]でクラスタを指定
nk <- function(listdata){
	X <- data.frame()
	for(i in listdata){
		X <- rbind(X, nrow(i))
	}
	X
}

#データを入れるとデータ数を返す
all_n <- function(data){
	d <- nrow(data)
	d
}

#各データから，同一クラスタ内の他のデータへの最小距離を求める
#入力：同一クラスタの全データ（正解クラスタ付き）
#出力：入力されたクラスタにおける平均距離

mini_dis <- function(c_data){
	n <- ncol(c_data)
	r <- nrow(c_data)
	d_data <- as.matrix(dist(c_data[,-n]))
	
	long <- 0
	for(n in 1:r){
		row_data <- d_data[n,]
		min_long <- min(row_data[-n])
		long <- long + min_long
	}
	mean_long <- long / r
	mean_long
}

#全データの，同一クラスタ内の他のデータへの最小距離の平均
#入力：正解付きデータ
#出力：平均最小距離
mean_min_dis <- function(data){
	listdata <- split_by_result(data)
	kn <- k_num(listdata)

	long <- 0
	for(i in 1:kn){
		dmin <- mini_dis(listdata[[i]])
		long <- long + dmin
	}
	mean_long <- long / kn
	mean_long
}

#複数のクラスタのデータを受けとって，クラスタ間の最小距離を出す
#入力：2つのクラスタのデータ
#出力：最小距離
cl_dis <- function(c1data, c2data){
	n <- ncol(c1data)
	c1 <- c1data[,-n]
	c2 <- c2data[,-n]

	c1_num <- nrow(c1data)
	c2_num <- nrow(c2data)
	
	min_long <- 0

	for(i in 1:c1_num){
		c1_i <- c1[i,]
		c2c1 <- rbind(c2, c1_i)
		m_data <- as.matrix(dist(c2c1))
		nn <- c2_num + 1
		min_can <- min(m_data[nn,-nn])

		if(i == 1){
			min_long <- min_can
		} else if (min_can < min_long) {
			min_long <- min_can
		}
	}
	min_long
}

#各クラスタの最近接クラスタへの最小距離の平均
#入力：正解付きデータ
#出力：最小距離の平均
cl_mean <- function(data){
	listdata <- split_by_result(data)
	kn <- k_num(listdata)
	
	long <- 0
	for(i in 1:kn){
		minlong <- 0
		for(j in 1:kn){
			if(i != j){
				dmin <- cl_dis(listdata[[i]], listdata[[j]])
				if((i == 1) || (j == 1)){
					minlong <- dmin
				} else if (dmin < minlong) {
					minlong <- dmin
				}
			}
		}
		long <- long + minlong
	}
	mean_long <- long / kn
	mean_long
}

#最小距離同士の掛け合わせ
#入力：正解付きデータ
#出力：値
min_index <- function(data){
	x <- cl_mean(data)
	y <- mean_min_dis(data)
	z <- y / x
	z
}







##################################################
dir_minindex <- function(){
	file_name <- list.files()

	tab <- lapply(list.files(pattern="tsv"), read.table)
	tab_num <- length(tab)

	all <- data.frame(MIN=0)

	for(i in 1:tab_num){
		m_i <- min_index(tab[[i]])
		all <- rbind(all, m_i)
	}
	file_name <- data.frame(file_name)
	all <- all[-1,]

	cbind(file_name,all)

}



