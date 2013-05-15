source("./sf.r")
source("./CA.r")

CA_loop <- function(inputdata, loop){
        ca_looped <- list()
        datanum <- nrow(inputdata) -1

        for (m in 1:loop){
                ca_list <- list()

                for (n in 1:datanum){
						km <- kmeans(inputdata, n)
						ca <- CA1(n, center_dis(inputdata), km$size, ca_wcd(n, addresult(inputdata,km,n),km))
                        ca_list[length(ca_list)+1] <- ca[[5]]
                }

                for(y in 1:length(ca_list)){
                        if (m ==1){
                                ca_looped <- ca_list
                        } else if (ca_looped[[y]] > ca_list[[y]]){
                                ca_looped[[y]] <- ca_list[[y]]
                        }
                }

        }
        ca_looped[length(ca_looped)+1] <- ca_looped[[1]]
        ca_looped
}





