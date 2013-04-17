###addresult###
#�������ǡ���(data, data.frame)��kmeans�η��(km)��k�ο�(k, integer)
#�֤��͡�list
#                       list[1] ��̤����äĤ���data.frame
#                       list[[2]][[n]] kmeans�η�̡�n���饹�����˳�����Ƥ�줿���Ǥ�����data.frame
addresult <- function(data, km, k){
        result_data <- cbind(data, km$cluster)
        result_list <- list(result_data)
        each_cluster <- split(result_data, km$cluster)
        listed <- list(result_list, each_cluster)
        listed
}

###WCD###
#������k�ο�(k, integer), addresult�ΰ���(listed, list)��kmeans�η��(km)
#�֤��͡�wcd����
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

###BCD###
#�������ǡ���(data, data.frame)��k�ο�(k, integer), kmeans�η��(km)
#�֤��͡�bcd����
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

###SI###
#������wcd,bcd����(www, bbb)
#�֤��͡�SI����
si <- function (www, bbb){
        ss <- exp(1)^exp(1)^(bbb-www)
        s <- 1 - (1/ss)
        s
}

###cluster = sample###
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

###SF###
sf <- function(loop, inputdata){
        sfc_looped <- list()
        datanum <- nrow(inputdata) -1

        for (m in 1:loop){
                sfc <- list()

                for (n in 1:datanum){
                        kms <- kmeans(inputdata, n)
                        ad <- addresult(inputdata, kms, n)
                        w_num <- wcd(n, ad, kms)
                        b_num <- bcd(inputdata, n, kms)
                        si_num <- si(w_num, b_num)
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



