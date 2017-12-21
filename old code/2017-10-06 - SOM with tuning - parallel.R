#SOM script for data exploration.
#This script outputs heatplots for each variable in the 'data1' dataset and saves them to disk.
#To run please ensure the following:
##1. The 'data' dataset is saved in the working directory.
##2. The filepath to save the heatplots to is as desired (row 59)

library(kohonen)
library(RColorBrewer)
library(colorRamps)
library(dplyr)
library(cluster)
library(foreach)
library(doParallel)


start<-proc.time()
set.seed(57)


#######################################################
#load in dataset
load("//nas01/analytics/Analytics Projects and Results/Segmentation using unsupervised learning/cc_data_for_segmentation.Rdata")
#data1<-data1_v11
#rm("data1_v11")

####################################################### USE THIS SECTION IF ONLY A SAMPLE OF THE DATA IS TO BE USED


#data1<-na.omit(data1)
data1<-player_df%>%

  select(max_day,active_days,max_session,gametime_hrs,furthest_level,revenue,num_purchases,gold_spent,
         num_level_attempts,num_unique_levels_attempted,avg_pass_rate,signed_in_to_Fb,num_event_rewards_won,max_event_reward_tier,
         num_distinct_coin_purchase_items,num_coins_purchased_via_iap,avg_coins_purchased_per_level,inactive_for_last_7_days,
         max_level_attempts,num_no_boost_attempts,num_no_boost_wins,coin_balance_range,num_stars_won,avg_stars_won_per_level,
         median_time_taken,has_decreasing_session_lengths,avg_session_length_mins)

data1[is.na(data1)]<-0
data1<-scale(data1)

bak<-data1
data1<-bak



###########


#create compute cluster
cl<-makeCluster(detectCores())
registerDoParallel(cl)

#non-spenders only

non_spenders<-player_df %>%
              filter(num_purchases==0)%>%
              select(max_day,active_days,max_session,gametime_hrs,furthest_level,gold_spent,
                     num_level_attempts,num_unique_levels_attempted,avg_pass_rate,signed_in_to_Fb,num_event_rewards_won,max_event_reward_tier,
                     num_distinct_coin_purchase_items,inactive_for_last_7_days,
                     max_level_attempts,num_no_boost_attempts,num_no_boost_wins,coin_balance_range,num_stars_won,avg_stars_won_per_level,
                     median_time_taken,has_decreasing_session_lengths,avg_session_length_mins)


non_spenders[is.na(non_spenders)]<-0
non_spenders<-scale(non_spenders)


bak_non_spenders<-non_spenders
non_spenders<-bak_non_spenders

default_som_length=round(sqrt(nrow(non_spenders)/10))

results <- foreach(j=1:8,.combine=rbind, .inorder = FALSE,.verbose=T) %dopar%{
  
  require(kohonen)
  require(RColorBrewer)
  require(colorRamps)
  require(dplyr)
  require(cluster)
  require(foreach)


#make parameter grid
xd=sample(c(default_som_length/4:default_som_length*3),1)
yd=sample(c(default_som_length/4:default_som_length*3),1)
rlen<-sample(c(50:5000),1)
alpha1<- (1-log10(runif(1,1.1,9.999)))*0.5
alpha2<- alpha1*(1-log10(runif(1,1.1,9.999)))*0.01
radius<-sample(5:15,1)
mode=sample(c("online", "batch"),1)

#aim for 10 instances per node
som_grid <- somgrid(xdim = xd, ydim=yd, topo="hexagonal")

set.seed(57)
non_spenders.som <- som(non_spenders, 
                 grid=som_grid, 
                 rlen=rlen, #number of iterations
                 alpha=c(alpha1,alpha2), #learning rate
                 radius=radius, # default is 2/3 of map length
                 mode=mode,
                 keep.data = TRUE)


#find optimum number of clusters - calculates dissimilarity between groups - look for highest value of s
s=NULL
for(i in 2:20){
  k=cutree(hclust(dist(non_spenders.som$codes[[1]]),method="ward.D"),i)
  s[i]=summary(silhouette(k,dist(non_spenders.som$codes[[1]])))$si.summary[3]
}

#results_list <- list(rlen,alpha1,alpha2,radius,mode,max(s[3:length(s)]))

data.frame(xdim=xd,ydim=yd,rl=rlen,a1=alpha1,a2=alpha2,r=radius,md=mode,S=max(s[3:length(s)]))


}

results<-arrange(results,desc(S))
write.xlsx(results,file="SOM_tuning_results.xlsx")

stopCluster(cl)
registerDoSEQ()

print(start-proc.time())


#######









# recreate winning SOM and plot

#aim for 10 instances per node
som_grid <- somgrid(xdim = 5.3, ydim=7.57, topo="hexagonal")

set.seed(57)
non_spenders.som <- som(non_spenders, 
                        grid=som_grid, 
                        rlen=2852, #number of iterations
                        alpha=c(0.319,6.259766e-04), #learning rate
                        radius=6, # default is 2/3 of map length
                        keep.data = TRUE)


s=NULL
for(i in 2:20){
  k=cutree(hclust(dist(non_spenders.som$codes[[1]]),method="ward.D"),i)
  s[i]=summary(silhouette(k,dist(non_spenders.som$codes[[1]])))$si.summary[3]
}


#create heatplots for each attribute
for ( i in (1:length(dimnames(non_spenders)[[2]]))){
  # png(paste0("SOM plots/",dimnames(non_spenders)[[2]][i],".png"))
  plot(non_spenders.som, type = "property", property = non_spenders.som$codes[[1]][,i],
       palette.name = colorRamps::blue2red,ann=FALSE,yaxt='n',xaxt='n',main="")
  title(main=dimnames(non_spenders)[[2]][i],cex.main=2)
  # dev.off()
}



#plot clusters
som_cluster <- cutree(hclust(dist(non_spenders.som$codes[[1]])), 9)
pretty_palette <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA",
                    "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411",
                    "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
#png("D:\\R code\\projects\\SOM\\clusters.png")
plot(non_spenders.som, bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(non_spenders.som, som_cluster)
#dev.off()

#evaluate quality of SOM (look for flattening graph)
plot(non_spenders.som,type="changes")


#plot counts map (number of instances at each node)
plot(non_spenders.som, main = "BQ Data - node counts",type="count")


#plot U matrix
plot(non_spenders.som, type="dist.neighbours",main="U-matrix")

