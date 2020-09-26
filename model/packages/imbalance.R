# 随机欠采样 在多数类中随机删除多数类
undersampling <- function(data, class){
	buggy_data <- data[which(data[class][,1] == "buggy"),]
	clean_data <- data[which(data[class][,1] == "clean"),]
	sampled_index <- sample(nrow(clean_data), nrow(buggy_data))
	sampled_data <- clean_data[sampled_index,]
	ret_data <- rbind(buggy_data, sampled_data)
	return(ret_data)
}
#随机过采样 在少数类中随机复制少数类
oversampling <- function(data, class){
	buggy_data <- data[which(data[class][,1] == "buggy"),]
	clean_data <- data[which(data[class][,1] == "clean"),]
	print(nrow(clean_data))
	print(nrow(buggy_data))
	sampled_index <- sample(nrow(buggy_data), nrow(clean_data), replace=TRUE)##随机复制少数类
	print(sampled_index)
	sampled_data <- buggy_data[sampled_index,]
	print(sampled_data)
	ret_data <- rbind(clean_data, sampled_data)
	return(ret_data)
}



