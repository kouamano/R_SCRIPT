
addresult <- function(data, km, k){
        result_data <- cbind(data, km$cluster)
        result_list <- list(result_data)
        each_cluster <- split(result_data, km$cluster)
        listed <- list(result_list, each_cluster)
        listed
}

ca_wcd <- function(k, listed, km){
        all_sq <- 0
        w <- 0
		waru <- numeric(k)
        for (i in 1:k){
                count <- 0
                for (j in 1:nrow(listed[[2]][[i]])){
                        row_1 <- listed[[2]][[i]][j,]
                        len <- length(row_1)
                        cutlast <- row_1[, -len[1]]
                        cluc <- rbind(km$centers[i,], cutlast)
                        dis <- dist(cluc)
                        count <- count + dis
                }
                waru[i] <- count/km$size[i]
        }
		waru
}

center_dis <- function(data){
	k1 <- kmeans(data, 1)
	all_dist <- 0
	
	data_num <- nrow(data)

	for(i in 1:data_num){
		d <- rbind(data[i,],k1$centers)
		ds <- dist(d)
		all_dist <- all_dist + ds[1]
	}
	cent <- all_dist / data_num

	cent
}

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

bcd <- function(data, k, km){
        ke1 <- kmeans(data, 1)

        bb <- 0
        for (i in 1:k){
                bind_center <- rbind(km$centers[i,], ke1$centers)
                d_center <- dist(bind_center)
                d2 <- (d_center[1]^2) * km$size[i]
                bb <- bb + d2
        }
        b <- bb / (k * nrow(data))
        b
}

si <- function (www, bbb){
        ss <- exp(1)^exp(1)^(bbb-www)
        s <- 1 - (1/ss)
        s
}

clus_samp <- function(data){
        km1 <-  kmeans(data, 1)
        ggg <- 0
        for(g in 1:nrow(data)){
                d <- rbind(data[g,], km1$centers)
                dt <- dist(d)
                d2 <- dt[1]^2
                ggg <- ggg + d2
        }
        g <- ggg / (nrow(data)*nrow(data))

        sg <- exp(1)^exp(1)^g
        sfg <- 1 - (1/sg)
        sfg
}

sf_single <- function(inputdata, k){
	kms <- kmeans(inputdata, k)
	ad <- addresult(inputdata, kms, k)
	w_num <- wcd(k, ad, kms)
	b_num <- bcd(inputdata, k, kms)
	si_s <- si(w_num, b_num)
	si_s
}

sf_loop <- function(inputdata, loop){
        sfc_looped <- list()
        datanum <- nrow(inputdata) -1

        for (m in 1:loop){
                sfc <- list()

                for (n in 1:datanum){
						si_num <- si_s(inputdata, n)
                        sfc[length(sfc)+1] <- si_num
                }
                for(y in 1:length(sfc)){
                        if (m ==1){
                                sfc_looped <- sfc
                        } else if (sfc_looped[[y]] < sfc[[y]]){
                                sfc_looped[[y]] <- sfc[[y]]
                        }
                }

        }
        sfc_looped[length(sfc_looped)+1] <- clus_samp(inputdata)
        sfc_looped
}
