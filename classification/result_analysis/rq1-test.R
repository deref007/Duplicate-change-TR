projects<-c("activemq","camel","derby","geronimo","hbase","hcommon","openjpa","pig")
#projects<-c("pig")
projectnames<-c("ActiveMQ","Camel","Derby","Geronimo","HBase","Hadoop.C.","Openjpa","Pig")
#projectnames<-c("ActiveMQ")
library(effsize)


szz_labels <- c("New")

compare_label <- "Old"

models <- c("Classifer", "Project")

result_matrix <- NULL

classifiers <- c("random_forest", "logistic_regression", "naive_bayes")
#classifiers<-c("exp")

root_path <- "C://Users//deref//Desktop//classification//result//oneway/"

# auc, f1measure, 
#"waste_effort","all_effort"
compare_measures <- c("oneway_r20")

models <- append(models, rep(c("In-dup", "Un-dup"), 2 * length(compare_measures)))

first_column <- c("RF", rep("", 7), "LR", rep("", 7), "NB", rep("", 7))
#first_column <- c("exp", rep("", 0))

get_project_name<-function(project){
	projectnames <- c("ActiveMQ","Camel","Derby","Geronimo","HBase","Hadoop.C.","Openjpa","Pig")
	#projectnames <- c("ActiveMQ")
	return(projectnames[which(projects==project)])
}

for (classifier in classifiers){
	for (p in projects){
		print(p)
		fn <- paste(c(root_path, p, "_","oneway", "_", compare_label, ".csv"), collapse="")
		data <- read.csv(fn)

		this_row <- c(get_project_name(p))

		for(compare_measure in compare_measures){
			for (szz_label in szz_labels){
				temp_fn <- paste(c(root_path, p, "_", "oneway", "_", szz_label, ".csv"), collapse="")
				temp_data <- read.csv(temp_fn)

				ret <- wilcox.test(data[compare_measure][,1], temp_data[compare_measure][,1],alternative="l", paired=TRUE)
				#ret <- wilcox.test(data[compare_measure][,1])
				print(ret)
				p_value <- ret["p.value"][[1]]
				ret2 <- cliff.delta(temp_data[compare_measure][,1], data[compare_measure][,1],return.dm=TRUE)
				# x<-c(temp_data$auc)
				# y<-c(data$auc)
				# ret2 <- cliff.delta(x,y,return.dm=TRUE)
				# ret2 <- cliff.delta(temp_data[compare_measure][,1])
				print(ret2)
				estimation <- ret2["estimate"][[1]]
				estimation <- round(estimation, 2)
				print(estimation)
				if (ret2["magnitude"] == 1){
					magnitude <- "(N)"
				}
				if (ret2["magnitude"] == 2){
					magnitude <- "(S)"
				}
				if (ret2["magnitude"] == 3){
					magnitude <- "(M)"
				}
				if (ret2["magnitude"] == 4){
					magnitude <- "(L)"
				}

				p_value_str <- ""
				adjusted_pvalue <- p_value * length(projects)
				if (adjusted_pvalue <= 0.001){
					p_value_str <- "***"
				}
				if (adjusted_pvalue <= 0.01 & adjusted_pvalue >0.001){
					p_value_str <- "**"
				}
				if(adjusted_pvalue <= 0.05 & adjusted_pvalue > 0.01){
					p_value_str <- "*"
				}

				estimation_str <- sprintf("%.2f", estimation+0.0000000001)
				this_row <- append(this_row, estimation_str)
				this_row <- append(this_row, paste(c(magnitude, p_value_str), collapse=""))
			}
		}
		if(is.null(result_matrix)){
			result_matrix <- matrix(this_row, nrow=1)
		}
		else{
			result_matrix <- rbind(result_matrix, matrix(this_row, nrow=1))
		}
	}
}

result_frame <- data.frame(result_matrix)
result_frame <- cbind(data.frame(first_column), result_frame)

# ge_path<-"C://Users//deref//Desktop//classification//result/"
# result_path<-paste(c(ge_path,"result_table/"))
# save_path <-paste(c(result_path, "rq1-balance-Pvalue", compare_measures, ".csv"), collapse="")
# write.csv(result_frame, save_path, row.names=FALSE)
print(result_frame)
