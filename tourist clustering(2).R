tour <- read.csv(file.choose(),header = T)

#install.packages('magrittr')
library(magrittr)
library(dplyr)

colnames(tour)

tour$daily_pay<-tour$total_pay/tour$period


######################################################################################
##################################### clustering #####################################
######################################################################################
######################################################################################

cluster_data <- tour %>% select('purpose','motivation1' ,'path_get_inform1','companion',
                                'num_companion', 'period','travel_type', 'date_survey',
                                'gender','job','age','daily_pay','total_pay')

cluster_data$age <- as.factor(cluster_data$age)
cluster_data$travel_type <- as.factor(cluster_data$travel_type)
cluster_data$job <- as.factor(cluster_data$job)
cluster_data$companion <- as.factor(cluster_data$companion)
cluster_data$date_survey <- as.factor(cluster_data$date_survey)
cluster_data$path_get_inform1 <- as.factor(cluster_data$path_get_inform1)
cluster_datagender <- as.factor(cluster_data$gender)
cluster_data$purpose <- as.factor(cluster_data$purpose)
cluster_data$motivation1 <- as.factor(cluster_data$motivation1)
cluster_data$gender <- as.factor(cluster_data$gender)




str(cluster_data)

################################feature selection#########################

cluster_data %<>% select('purpose','travel_type','daily_pay','companion')




 
#####################clustering modeling##################
library(cluster)

gower_dist<-daisy(cluster_data, metric="gower",stand=T)
sil_width<-c(NA)

for(i in 2:6){
  pam_fit <- pam(gower_dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

#install.packages('ggplot')

library(ggplot2)
df <- data.frame(x = 1:6, y = sil_width)
df %>% ggplot(aes(x = x, y = y)) + geom_point(size = 3, col = 'red') + geom_line( size = 1) 

sil_width[6]

pam_fit <- pam(gower_dist, diss = TRUE, k = 6)
cluster_data[pam_fit$medoids,]

cluster_data$cluster <- pam_fit$clustering


group1<-cluster_data[which(pam_fit$clustering==1),]
group2<-cluster_data[which(pam_fit$clustering==2),]
group3<-cluster_data[which(pam_fit$clustering==3),]
group4<-cluster_data[which(pam_fit$clustering==4),]
group5<-cluster_data[which(pam_fit$clustering==5),]
group6<-cluster_data[which(pam_fit$clustering==6),]

######################result of clustering##########

summary(group1)
summary(group2)
summary(group3)
summary(group4)
summary(group5)
summary(group6)

###################evaluation#################

##1) visualization
install.packages('Rtsne')
library(Rtsne)
tsne_obj = Rtsne(gower_dist,is_distance = TRUE)
tour_plot = data.frame(tsne_obj$Y)
names(tour_plot) = c("X","Y")
tour_plot$cluster <- as.factor(pam_fit$clustering)
ggplot(aes(x=X, y=Y), data=tour_plot) + geom_point(aes(color=cluster), alpha = 0.5, size = 1.5) + theme_bw()


tour$cluster<-pam_fit$clustering


table(tour$date_survey)
table(tour$season)

spring<-which(tour$date_survey==3|tour$date_survey==4|tour$date_survey==5)
summer<-which(tour$date_survey==6|tour$date_survey==7|tour$date_survey==8)
fall<-which(tour$date_survey==9|tour$date_survey==10|tour$date_survey==11)
winter<-which(tour$date_survey==12|tour$date_survey==1|tour$date_survey==2)

tour$season<-1

colnames(tour)
table(tour$recommend_intention)
table(tour$revisit_intention)
table(tour$Y)

bad<-which(tour$revisit_intention==1|tour$revisit_intention==2|tour$revisit_intention==3)
good<-which(tour$revisit_intention==4|tour$revisit_intention==5)
tour$Y<-NA
tour[bad,48]<-0
tour[good,48]<-1

tour[spring,47]<-1
tour[summer,47]<-2
tour[fall,47]<-3
tour[winter,47]<-4

setwd('C:/Users/dhy03/Desktop')


group1_data<-tour[which(tour$cluster==1),]
group2_data<-tour[which(tour$cluster==2),]
group3_data<-tour[which(tour$cluster==3),]
group4_data<-tour[which(tour$cluster==4),]

write.csv(group1_data,file='group1_data.csv',row.names = FALSE)
write.csv(group2_data,file='group2_data.csv',row.names = FALSE)
write.csv(group3_data,file='group3_data.csv',row.names = FALSE)
write.csv(group4_data,file='group4_data.csv',row.names = FALSE)

write.csv(tour,file='cluster_season_finaldat.csv',row.names = FALSE)

##2) silhouette function
spam <- silhouette(pam_fit)
plot(spam)
