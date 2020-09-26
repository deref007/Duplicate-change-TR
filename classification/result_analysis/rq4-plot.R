
library("ggplot2")

#projects <- c("activemq", "camel", "geronimo", "hcommon", "hbase",  "pig")
#projects<-c( "activemq", "camel", "derby","geronimo", "hcommon", "hbase", "openjpa", "pig")
projects<-c("activemq","camel","derby","geronimo","hbase","hcommon","openjpa","pig")
szz_labels <- c("buggy_AG")

classifier <- "random_forest"
#c("random_forest", "logistic_regression", "naive_bayes")

compare_label <- "buggy_RA"	

result_matrix <- matrix(c("projects", "color", "SZZ", "Rank", "Shifts"), nrow=1)

root_path <- "C://Users//deref//Desktop//classification//result//"

rank_n_features <- function(importance_data, szz_label, n){##返回排名n的特征名字
	szz_group <- paste(szz_label, "_groups", sep="")
	temp_data <- importance_data[which(importance_data[szz_group][,1] == n),]
	szz_feature <- paste(szz_label, "_features", sep="")
	features <- temp_data[szz_feature][,1]
	return(features)
}


corresponding_project <- function(project_name){##提取项目
	projectnames <- c("ActiveMQ", "Camel",  "Derby","Geronimo", "Hadoop C.", "HBase", "Openjpa", "Pig")

	pa <- projectnames[which(projects==project_name)]

	return(pa)
}

corresponding_label <- function(szz_label){  ##标签
	corresponding_labels <- c("AG")
	return(corresponding_labels[which(szz_labels==szz_label)])
}


shift_n_features <- function(importance_data, szz_label1, szz_label2, n){
	fe_number <- nrow(importance_data)#行数
	szz_group1 <- paste(szz_label1, "_groups", sep="")#返回的是标签1的排名
	szz_group2 <- paste(szz_label2, "_groups", sep="")##返回的是标签2的排名
	szz_feature1 <- paste(szz_label1, "_features", sep="")##返回的是特征
	szz_feature2 <- paste(szz_label2, "_features", sep="")
	rank_n_features1 <- rank_n_features(importance_data, szz_label1, n)
	rank_n_features2 <- rank_n_features(importance_data, szz_label2, n)
	shifts <- 0
	for (fe in rank_n_features1){
		temp_data <- importance_data[which(importance_data[szz_feature2][,1]== fe),]
		temp_rank <- temp_data[szz_group2][1,1]
		temp_rank[is.na(temp_rank)] <- 0
		shifts <- shifts + abs(temp_rank - n)
	}

	for (fe in rank_n_features2){
		temp_data <- importance_data[which(importance_data[szz_feature1][,1] == fe),]
		temp_rank <- temp_data[szz_group1][1,1]
		temp_rank[is.na(temp_rank)] <- 0
		if (temp_rank > 4){
			shifts <- shifts + abs(temp_rank - n)
		}
	}
	return(shifts / fe_number)
}

for (p in projects){
	importance_fn <- paste(c(root_path, "New_importance/", p, "_", classifier, "_importance.csv"), collapse="")
	importance_data <- read.csv(importance_fn)
	print(importance_data)

	for (szz_label in szz_labels){
		if(szz_label != compare_label){
			shifts1 <- shift_n_features(importance_data, szz_label, compare_label, 1)
			shifts2 <- shift_n_features(importance_data, szz_label, compare_label, 2)
			shifts3 <- shift_n_features(importance_data, szz_label, compare_label, 3)
			project_name <- corresponding_project(p)
			szz <- corresponding_label(szz_label)
			temp_matrix <- matrix(c(project_name, I("red"), szz, "Rank1    ", shifts1), nrow=1)
			result_matrix <- rbind(result_matrix, temp_matrix)
			temp_matrix <- matrix(c(project_name, I("green"), szz, "Rank2    ", shifts2), nrow=1)
			result_matrix <- rbind(result_matrix, temp_matrix)
			temp_matrix <- matrix(c(project_name, I("blue"),szz, "Rank3    ", shifts3), nrow=1)
			result_matrix <- rbind(result_matrix, temp_matrix)
		}
	}
}

result_frame <- data.frame(result_matrix[-1,])
names(result_frame) <- c("projects", "Rank", "label", "SZZ_Rank", "Shifts")
print(result_frame)



result_frame$Shifts <- as.numeric(as.character(result_frame$Shifts))
result_frame$label <- factor(result_frame$label, order=TRUE, levels=c("AG"))

result_frame <- result_frame[order(result_frame$label),]

tempv <- c()

library("effsize")

for (l in c("AG")){
	print(l)
	temp_result_frame <- result_frame[which(result_frame$label == l),]
	for (rank in unique(result_frame$SZZ_Rank)) {
		print(rank)
		print("mean_value:")
		print(mean(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts))
		print("median value:")
		print(median(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts))
		print(var(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts))
		temp_vec <- c(1:10) - c(1:10)
		print(wilcox.test(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts, temp_vec, alternative="g"))
		print(cliff.delta(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts, temp_vec))
	}
}

#
p <- ggplot(result_frame, aes(projects, Shifts, color=I("black"), fill=SZZ_Rank)) + geom_bar(stat="identity", position="dodge", width=0.5)

p2 <- p +  theme(panel.grid.major=element_line(color="grey90")) + theme(panel.grid.minor=element_line(color="grey90"))  #+ theme(panel.grid.major=element_blank()) # + theme(panel.grid.minor=element_blank())

p3 <- p2 + xlab("") + ylab("Rank Shifts") + theme(axis.title=element_text(size=12, face="bold", color=I("black")))


p4 <- p3 + theme(axis.text.y=element_text(size=12, color="black")) + theme(strip.text=element_text(size=12))

p5 <- p4 + theme(strip.background=element_rect(fill="grey70", color="grey60")) + theme(panel.background=element_rect(fill="white", color="grey40"))

p6 <- p5 + theme(axis.text.x=element_text(size=12, color="black", angle=90, hjust=1, vjust=0.5)) + theme(legend.position="top")

p7 <- p6 + scale_fill_manual("", values=c(I("brown3"), I("green4"), I("lightblue")))

#p7 <- p6 + scale_colour_brewer(palette="S")

p8 <- p7 + theme(legend.background=element_rect(color=I("black"))) + theme(legend.text=element_text(size=12, color="black"))

# size_legend<-guide_legend(title = 'legend', title.position = "left", title.hjust =  .5, title.vjust = .5, title.theme = element_text(size = 15,face = "italic",colour = "red",angle = 45),
#   keywidth = 2.5,
#   keyheight = 2,
#   label.position = "bottom",
#   direction = "horizontal",
#   label = TRUE,
#   label.hjust = 0.5,
#   label.vjust = 0.5)
result_path<-paste(c(root_path,"result_table/"))
ggsave(paste(c(result_path, classifier,  "rq4-2_.pdf"), collapse=""), width=8, height=3)
