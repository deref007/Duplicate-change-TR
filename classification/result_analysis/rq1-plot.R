library("ggplot2")

projects <- c("activemq", "camel", "hcommon", "hbase", "pig")

szz_labels <- c("New")

root_path <- "C://Users//deref//Desktop//classification//result/"

classifier <- "random_forest"

compare_szz_label <- "Old"

result_data_frame <- NULL

# "auc", "brier", "precision", "recall", "f1measure", "precision20", "recall20", "f1measure20", "IFA", "PCI20"
compare_measure <- "auc"

folds <- 1000


pure_vector <- function(string){
	a <- c()
	for (i in 1:folds){
		a <- append(a, string)
	}
	return(a)
}

corresponding_project <- function(project_name){
	projectnames <- c("ActiveMQ", "Camel",  "Hadoop C.", "HBase",  "Pig")

	pa <- projectnames[which(projects==project_name)]

	return(pa)
}

corresponding_label <- function(szz_label){
	corresponding_labels <- c("New")
	return(corresponding_labels[which(szz_labels==szz_label)])
}


for (p in projects) {
	temp_frame <- NULL
	compare_result_fn <- paste(c(root_path, "imbalance/", p, "_", classifier, "_", compare_szz_label, ".csv"), collapse="")
	compare_result <- read.csv(compare_result_fn)
	for (szz_label in szz_labels){
		result_fn <- paste(c(root_path, "imbalance/", p, "_", classifier, "_", szz_label, ".csv"), collapse="")
		result <- read.csv(result_fn)
		ratio <- result[compare_measure][,1] / compare_result[compare_measure][,1]
		project_names <- pure_vector(corresponding_project(p))
		SZZ <- pure_vector(corresponding_label(szz_label))
		if (is.null(temp_frame)){
			temp_frame <- data.frame(ratio, project_names, SZZ)
		}
		else{
			temp_frame <- rbind(temp_frame, data.frame(ratio, project_names, SZZ))
		}
	}
	
	if (is.null(result_data_frame)){
		result_data_frame <- temp_frame
	}
	else{
		result_data_frame <- rbind(result_data_frame, temp_frame)
	}
}
print(result_data_frame)

p2 <- ggplot(result_data_frame, aes(SZZ, ratio, color=I("black"), fill=I("lightblue"))) + geom_violin() + facet_wrap(~project_names, ncol=10)

p3 <- p2 + stat_summary(fun.y=median, geom="point", size=2, color="black")

p4 <- p3 + geom_hline(aes(yintercept=1), linetype="dashed", size=0.5, color="red")

p4 <- p4 + geom_hline(aes(yintercept=0.95), linetype="dotdash", size=0.5, color="blue")

p5 <- p4 + theme(panel.grid.major=element_line(color="grey90")) + theme(panel.grid.minor=element_line(color="grey90")) + theme(strip.text=element_text(size=12))

p5 <- p5 + ylab("Ratio to Model Old") + xlab("Models")

p6 <- p5 + theme(axis.text.y=element_text(size=12, color="black")) + theme(axis.title=element_text(face="bold", size=14))

p7 <- p6 + theme(strip.background=element_rect(fill="grey70", color="grey40")) + theme(panel.background=element_rect(fill="white", color="grey40"))


p7 + theme(axis.text.x=element_text(size=12, color="black")) 

#+ scale_y_continuous(limits=c(0.9, 1.02))

ggsave(paste(c(root_path, classifier, "_", compare_measure, ".jpg"), collapse=""), width=15, height=3)
