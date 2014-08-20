source("./min_index.r")
source("./mst_index.r")

#カレントディレクトリ内にある全てのtsvに尺度を適用

direct_allfile_app_index <- function(p=2){
	file_name <- list.files()

	data_with_ans <- lapply(list.files(pattern="tsv"), read.table)
	data_num <- length(data_with_ans)

	all <- data.frame(MIN=0, MIN2=0, MIN3=0, MIN4=0, MIN5=0, MST=0, MST2=0, MST3=0)

	for(i in 1:data_num){
		m_i <- min_index(data_with_ans[[i]])
		m2_i <- min2_index(data_with_ans[[i]])
		m3_i <- min3_index(data_with_ans[[i]])
		m4_i <- min4_index(data_with_ans[[i]])
		m5_i <- min5_index(data_with_ans[[i]])
		ms1_i <- mst1_index(data_with_ans[[i]])
		ms2_i <- mst2_index(data_with_ans[[i]])
		ms3_i <- mst3_index(data_with_ans[[i]])
		all <- rbind(all, c(m_i, m2_i, m3_i, m4_i, m5_i, ms1_i, ms2_i,ms3_i))
	}

	all <- all[-1,]
	cbind(file_name,all)
}





