
general_root_path <- "C://Users//deref//Desktop//classification//result/"
root_path <- paste(general_root_path, "dup_data2/", sep="")

projects <- c("activemq", "camel", "geronimo", "hcommon", "hbase",  "pig")

all_data <- NULL
for (p in projects){
    p_path <- paste(c(root_path, p, ".csv"), collapse="")
	
	data <- read.csv(p_path)
	
	if (is.null(all_data)){
		all_data <- data
	}
	else{
		all_data <- rbind(all_data, data)
	}
}

all_data$churn_size <- all_data$la + all_data$ld

#buggy_B_changes <- all_data[which(all_data$buggy_B2=="buggy" & all_data$buggy_RA == "clean"),]
#buggy_AG_changes <- all_data[which(all_data$buggy_AG=="buggy" & all_data$buggy_RA == "clean" ),]
# buggy_MA_changes <- all_data[which(all_data$buggy_MA=="buggy" & all_data$buggy_RA == "clean"),]
buggy_RA_changes <- all_data[which(all_data$buggy_RA=="buggy" & all_data$buggy_B2 == "clean"),]
buggy_RA_changes2<- all_data[which(all_data$buggy_RA=="buggy" & all_data$buggy_AG == "clean"),]
buggy_RA_changes3 <- all_data[which(all_data$buggy_RA=="buggy" & all_data$buggy_MA== "clean"),]


boxplot(buggy_B_changes$churn_size, buggy_RA_changes$churn_size, ylim=c(0,1000))
boxplot(buggy_AG_changes$churn_size, buggy_RA_changes2$churn_size, ylim=c(0,1000))
boxplot(buggy_MA_changes$churn_size, buggy_MA_changes$churn_size, ylim=c(0,1000))

wilcox.test(buggy_B_changes$churn_size, buggy_RA_changes$churn_size, paired=FALSE, alternative="g")

library(effsize)
cliff.delta(buggy_B_changes$churn_size, buggy_RA_changes$churn_size)