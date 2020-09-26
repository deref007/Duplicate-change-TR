# Title     : TODO
# Objective : TODO
# Created by: deref
# Created on: 2020/4/10
projects<-c("activemq","camel","derby","geronimo","hbase","hcommon","openjpa","pig")
projectnames<-c("ActiveMQ","Camel","Derby","Geronimo","HBase","Hadoop C.","Openjpa","Pig")

library(effsize)


szz_label <- c("New")

compare_label <- "Old"

models <- c("Classifer", "Project")

result_matrix <- NULL

#classifiers <- c("random_forest", "logistic_regression", "naive_bayes")
classifier<-c("exp")

root_path <- "C://Users//deref//Desktop//classification//result//exp_difference/"

# auc, f1measure,
#"waste_effort","all_effort"
p<-"pig"
fn <- paste(c(root_path, p, "_", classifier, "_", compare_label, ".csv"), collapse="")
data <- read.csv(fn)
data<-data$SEXP
print(data)
temp_fn <- paste(c(root_path, p, "_", classifier, "_", szz_label, ".csv"), collapse="")
temp_data <- read.csv(temp_fn)
temp_data<-temp_data$SEXP
print(temp_data)
ret<-wilcox.test(data,temp_data,paired=TRUE,alternative="g")
print(ret)
print(ret["p.value"][[1]])
ret2 <- cliff.delta(temp_data, data)
print(ret2)
#ret <- wilcox.test(temp_data[compare_measure][,1], data[compare_measure][,1], alternative="l", paired=TRUE)
# ret <- wilcox.test(temp_data[compare_measure][,1])
#
# print(ret)
# p_value <- ret["p.value"][[1]]
# print(p_value)
# ret2 <- cliff.delta(temp_data[compare_measure][,1], data[compare_measure][,1],return.dm=TRUE)
# 				# x<-c(temp_data$auc)
# 				# y<-c(data$auc)
# 				#ret2 <- cliff.delta(x,y,return.dm=TRUE)
# 				#ret2 <- cliff.delta(temp_data[compare_measure][,1])
# print(ret2)


