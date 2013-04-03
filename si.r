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
#�����F�f�[�^(data, data.frame)�Akmeans�̌���(km)�Ak�̐�(k, integer)
#�Ԃ�l�Flist
#			list[1] ���ʂ���������data.frame
#			list[[2]][[n]] kmeans�̌��ʁAn�N���X�^�[�Ɋ��蓖�Ă�ꂽ�v�f������data.frame
		
addresult <- function(data, km, k){
	result_data <- cbind(data, km$cluster)
	result_list <- list(result_data)
	each_cluster <- split(result_data, km$cluster)
	listed <- list(result_list, each_cluster)
	listed
}

############WCD############
#�����Fk�̐�(k, integer), addresult�̈���(listed, list)�Akmeans�̌���(km)
#�Ԃ�l�Fwcd�̒l
wcd <- function(k, listed){
	#�N���X�^�ԍ����ǉ�
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
#�����F
#�Ԃ�l�F
bcd <- function(){


}

git add file
git commit
git push origin master




