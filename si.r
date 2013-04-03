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
	#クラスタ番号分追加
	cent <- transform(km$center, center = 0)
	for (i in 1:k){
		count <- 0
		for (j in 1:nrow(listed[[2]][[i]])){
			cluc_data <- rbind(cent[i,], listed[[2]][[i]][j,])
			count <- length(cluc_data)
			cluc <- cluc_data[, -count]
			dis <- dist(cluc)
			dis2 <- dis^2
			count <- count + dis2
		}
		waru <- count/km$size[i]
		sq=sqrt(waru)
		all_sq <- sq + all_sq
	}
	wcd <- all_sq / k
	wcd
}



############BCD############
#引数：
#返り値：
bcd <- function(){


}

git add file
git commit
git push origin master




