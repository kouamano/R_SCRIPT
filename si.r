#SI index in R

#> class(k$cluster)
#[1] "integer"
#> class(k$centers)
#[1] "matrix"
#> class(k$totss)
#[1] "numeric"
#> class(k$withinss)
#[1] "numeric"
#> class(k$tot.withinss)
#[1] "numeric"
#> class(k$betweenss)
#[1] "numeric"
#> class(k$size)
#[1] "integer"
#> class(k)
#[1] "kmeans"


#data <-read.table("C:/Users/ww/Documents/cluster/noisy7.tsv")
#k <- 5
#km <- kmeans(data, k)


######## add result #######
#引数：データ(data, data.frame)、kmeansの結果(km)、kの数(k, integer)
#返り値：list
#			list[1] 結果がくっついたdata.frame
#			list[[2]][[n]] kmeansの結果、nクラスターに割り当てられた要素だけのdata.frame
		
addresult <- function(data, km, k){
	result_data <- cbind(data, km$cluster)
	result_list <- list(result_data)
	each_cluster <- split(result_data, km$cluster)
	listed <- list(result_list, each_cluster)
	listed
}

############WCD############
#引数：kの数(k, integer), addresultの引数(listed, list)、kmeansの結果(km)
#返り値：wcdの値
wcd <- function(k, listed, km){
	all_sq <- 0
	w <- 0
	for (i in 1:k){
		count <- 0
		for (j in 1:nrow(listed[[2]][[i]])){
			row_1 <- listed[[2]][[i]][j,]
			len <- length(row_1)
			cutlast <- row_1[, -len[1]]
			cluc <- rbind(km$centers[i,], cutlast)
			dis <- dist(cluc)
			dis2 <- dis[1]^2
			count <- count + dis2
		}
		waru <- count/km$size[i]
		sq = sqrt(waru)
		all_sq <- all_sq + sq
	}
	w <- all_sq / k
	w
}

############BCD############
#引数：データ(data, data.frame)、kの数(k, integer), kmeansの結果(km)
#返り値：bcdの値
bcd <- function(data, k, listed, km){
	ke1 <- kmeans(data, 1)
	
	bb <- 0
	for (i in 1:k){
		bind_center <- rbind(km$centers[i,], ke1$centers)
		d_center <- dist(bind_center)
		d2 <- (d_center[1]^2) * km$size[i]
		bb <- bb / d2
	}
	b <- bb / (k * nrow(data))
	b
}

############SI############
#引数：wcd,bcdの値(www, bbb)
#返り値：SIの値
si <- function (www, bbb){
	ss <- exp(1)^exp(1)^(bbb-www)
	s <- 1 - (1/s)
	s
}





git add file
git commit
git push origin master




