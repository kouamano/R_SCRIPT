
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

#各データから，同一クラスタ内の他のデータへの最小距離の平均を求める
#入力：同一クラスタの全データ（正解クラスタ付き）
#出力：入力されたクラスタにおける最小距離平均
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
		fl <- 0
		for(j in 1:kn){
			if(i != j){
				dmin <- cl_dis(listdata[[i]], listdata[[j]])
				if(fl == 0){
					minlong <- dmin
					fl <- 1
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
#出力：値　小さいほどよい
min_index <- function(data){
	x <- cl_mean(data)
	y <- mean_min_dis(data)
	z <- y / x
	z
}

#各データから，同一クラスタ内の他のデータへの最小距離の平均を指数にする
#入力：同一クラスタの全データ（正解クラスタ付き）
#出力：（１/データ数）＾mini_dis
mini2_dis <- function(c_data){
	n <- ncol(c_data)
	r <- nrow(c_data)

	if(r == 1){
		dd <- 0
		dd
	}else{
		d_data <- as.matrix(dist(c_data[,-n]))
	
		long <- 0
		for(n in 1:r){
			row_data <- d_data[n,]
			min_long <- min(row_data[-n])
			long <- long + min_long
		}
		dd <- 1 / r
		ddd <- dd^long
		ddd
	}
}

#全データの，同一クラスタ内の他のデータへの最小距離の平均(指数版)
#入力：正解付きデータ
#出力：平均最小距離
mean2_min_dis <- function(data){
	r <- nrow(data)
	listdata <- split_by_result(data)
	kn <- k_num(listdata)

	long <- 0
	for(i in 1:kn){
		dmin <- mini2_dis(listdata[[i]])
		long <- long + dmin
	}
	d <- long / kn
	dd <- (1/kn)^d
	dd
}

cl2_mean <- function(data){
	listdata <- split_by_result(data)
	kn <- k_num(listdata)
	
	long <- 0
	for(i in 1:kn){
		minlong <- 0
		fl <- 0
		for(j in 1:kn){
			if(i != j){
				dmin <- cl_dis(listdata[[i]], listdata[[j]])
				if(fl == 0){
					minlong <- dmin
					fl <- 1
				} else if (dmin < minlong) {
					minlong <- dmin
				}
			}
		}
		long <- long + minlong
	}
	long <- long / kn
	d <- 1 / kn
	dd <- d^long
	dd
}

#尺度2つめ, 小さいほどよい
min2_index <- function(data){
	x <- cl2_mean(data)
	y <- mean2_min_dis(data)
	z <- x * y
	z
}

#min_indexの分子だけ使用
#小さいほどよい
min3_index <- function(data){
	listdata <- split_by_result(data)
	kn <- k_num(listdata)

	x <- mean_min_dis(data)
	x
}

#min2_indexの分子の一部のみ
#大きいほどよい
min4_index <- function(data){

	r <- nrow(data)
	listdata <- split_by_result(data)
	kn <- k_num(listdata)

	long <- 0
	for(i in 1:kn){
		dmin <- mini4_dis(listdata[[i]])
		long <- long + dmin
	}
	d <- long
	d
}


mini4_dis <- function(c_data){
	n <- ncol(c_data)
	r <- nrow(c_data)

#	d_mean <- mean(dist(c_data[,-n]))
#正規化は当面考えない

	if(r == 1){
		dd <- 0
		dd
	}else{
		d_data <- as.matrix(dist(c_data[,-n]))
	
		long <- 0
		for(n in 1:r){
			row_data <- d_data[n,]
			min_long <- min(row_data[-n])
#			min_long <- min_long / d_mean
			long <- long + min_long
		}
		dd <- 1 / r
		ddd <- dd^long
		ddd
	}
}


#小さいほどよい
min5_index <- function(data){
	r <- nrow(data)
	listdata <- split_by_result(data)
	kn <- k_num(listdata)

	long <- 0
	for(i in 1:kn){
		dmin <- mini5_dis(listdata[[i]])
		long <- long + dmin
	}
	
	z <- long / kn
	z
}

mini5_dis <- function(c_data){
	n <- ncol(c_data)
	r <- nrow(c_data)
	d_data <- as.matrix(dist(c_data[,-n]))
	
	long <- 0
	for(n in 1:r){
		row_data <- d_data[n,]
		min_long <- min(row_data[-n])
		long <- long + min_long
	}
	long
}






##################################################
dir_minindex <- function(p=2){
	file_name <- list.files()

	tab <- lapply(list.files(pattern="tsv"), read.table)
	tab_num <- length(tab)

	all <- data.frame(MIN=0, MIN2=0, MIN3=0, MIN4=0, MIN5=0)

	for(i in 1:tab_num){
		m_i <- min_index(tab[[i]])
		m2_i <- min2_index(tab[[i]])
		m3_i <- min3_index(tab[[i]])
		m4_i <- min4_index(tab[[i]])
		m5_i <- min5_index(tab[[i]])
		all <- rbind(all, c(m_i, m2_i, m3_i, m4_i, m5_i))
	}
	file_name <- data.frame(file_name)
	all <- all[-1,]

	cbind(file_name,all)

}








