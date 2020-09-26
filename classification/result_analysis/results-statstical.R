library(xtable)
library(effsize)

root_path <- "C://Users//deref//Desktop//classification//results_balance/"

classifiers <-c("random_forest", "logistic_regression", "naive_bayes")

classifier_col <- c("RF", rep("", 5), "LR", rep("", 5), "NB", rep("", 5))

projects <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")
projectnames <- c("ActiveMQ", "Camel", "Derby", "Geronimo", "Hadoop C.", "HBase", "Mahout", "OpenJPA", "Pig", "Tuscany")

szz_labels <- c( "buggy_AG","buggy_RA")

measures <- c("auc")

framenames <- c("Classifier", "Project", rep(c( "AG", "RA"), length(measures)-1), c( "AG","RA"))

result_matrix <- NULL

result_matrix2 <- NULL

szz_baseline <- "buggy_RA"


for (classifier in classifiers){
	for (p in projects){
		this_row <- c()
		this_row2 <- c()
		baseline_result_fn <- paste(c(root_path, p, "_", classifier, "_", szz_baseline, ".csv"), collapse="")
		baseline_results <- read.csv(baseline_result_fn)
		for (measure in measures){
			for (szz_label in szz_labels){
				result_fn <- paste(c(root_path, p, "_", classifier, "_", szz_label, ".csv"), collapse="")
				results <- read.csv(result_fn)
				results[is.na(results)] <- 0
 				med_value <- mean(results[measure][,1])
				med <- sprintf("%.2f", med_value + 0.0000000001)

				ret <- wilcox.test(baseline_results[measure][,1], results[measure][,1], alternative="g", paired=TRUE)
				p_value <- ret["p.value"][[1]]

				ret3 <- wilcox.test(baseline_results[measure][,1], results[measure][,1], alternative="l", paired=TRUE)
				p_value2 <- ret3["p.value"][[1]]

				adjusted_pvalue <- p_value * length(projects)
				adjusted_pvalue2 <- p_value2 * length(projects)
				p_str <- ""
				if (adjusted_pvalue < 0.05){
					p_str <- I("\\ $\\searrow$")
				}

				if (adjusted_pvalue2 < 0.05){
					p_str <- I("")
				}

				ret2 <- cliff.delta(results[measure][,1], baseline_results[measure][,1])
				estimation <- ret2["estimate"][[1]]
				estimation_str <- sprintf("%.2f", estimation + 0.0000000001)
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

				this_row <- append(this_row, paste(med, p_str, sep=" "))
				if (szz_label != szz_baseline){
					if (adjusted_pvalue < 0.05){
						this_row <- append(this_row, paste(estimation_str, magnitude, sep="\\ "))
					}
					else{
						this_row <- append(this_row, "-")
					}
				}

				this_row2 <- append(this_row2, )
			}
		}

		if (is.null(result_matrix)){
			result_matrix <- matrix(this_row, nrow=1)
		}
		else{
			result_matrix <- rbind(result_matrix, matrix(this_row, nrow=1))
		}
	}
}

result_frame <- data.frame(result_matrix)
result_frame <- cbind(data.frame(rep(projectnames,3)), result_frame)
result_frame <- cbind(data.frame(classifier_col), result_frame)
names(result_frame) <- framenames

latex_table <- xtable(result_frame)
align(latex_table) <- ""
print(latex_table, include.rownames=FALSE, sanitize.text.function=function(x){x})


# col1 <- paste(format(as.numeric(result_matrix[,1]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")
# col2 <- paste(format(as.numeric(result_matrix[,2]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")
# col3 <- paste(format(as.numeric(result_matrix[,3]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")

# result_matrix1 <- cbind(result_matrix[,1], matrix(col1))
# result_matrix2 <- cbind(result_matrix[,2], matrix(col2))
# result_matrix3 <- cbind(result_matrix[,3], matrix(col3))
# result_matrix4 <- cbind(result_matrix1, cbind(result_matrix2, cbind(result_matrix3, result_matrix[,4])))

# result_frame <- data.frame(result_matrix4)
# result_frame <- cbind(data.frame(rep(projectnames,3)), result_frame)
# result_frame <- cbind(data.frame(classifier_col), result_frame)
# names(result_frame) <- framenames

# latex_table <- xtable(result_frame)
# align(latex_table) <- ""
# print(latex_table, include.rownames=FALSE)

