# ISSUES - THE PARAMETERS HAVE BEEN CAREFULLY CLAMPED OR ELSE THE TUNING ALGORITHM SEEKS A 'THIN' MAP WITH ONLY LARGE SCALE CLUSTERING
#NOT NECESSARILY AN ISSUE BUT LIMITING ASPECT RATIO AND RADIUS SEEMS TO SOLVE IT
#THE MIN RLEN PARAMETER MUST BE CHOSEN REALLY CAREFULLY. FOR THE FULL DATASET AT LEAST 1000 IS NEEDED OR THE MODEL DOESNT CLUSTER SUFFICIENTLY (IT
#BASICALLY FINDS TWO CLUSTERS BECAUSE IT IS ACTING TOO BROADLY). BUT INCREASING DRASTICALLY INCREASES RUNTIME

#TODO - DROP PAYING USERS INTO THE MAP AT INTERVALS BEFORE THEY PAY (USE FUNCTION FROM SHARED LIBRARY TO GET EVENTS BEFORE THEY PAY)
#LOOKS LIKE THE MAP.KOHONEN FUNCTION DOES THIS. IF THAT FAILS, CAN GET CODEBOOK VECTORS (getCodes) AND FIND NEAREST NODE TO EACH INPUT

#SOM script for data exploration.
#to use, select parameters and whether sampling is used (top section)


library(kohonen)
library(RColorBrewer)
library(colorRamps)
library(dplyr)
library(cluster)
library(foreach)
library(doParallel)

#GA paramaters
use_sample<-F
sample_size<-5000
iterations<-50 # iterations (ie generations) for GA 24 default
individuals_per_generation <-100 #number of individuals created for each iteration 96 default (note if this is too low (about 24) a sampling error will arise)
mutate_threshold<-0.6 #chance of a given parameter being mutated in mutate case default 0.8
elites_to_carry_forward<-3 #top 'n' individuals carried forward from each generation default 3
elitism_proportion<-5 #recipricol of fraction of population that are considered elite (default 10)


cross_probability<-0.3
mutate_probability<-0.9
suboptimal_carry_forward<-1

min_clusters<-5
min_radius_recip<-10 #reciprocal of fraction of the length of the map the radius cannot fall below
max_radius_recip<-2
min_rlen<-1000 #This is absolutely critical - too low and we get a map optimising on a single split. Too high and the runtime is too long
max_rlen<-2000
max_map_aspect_ratio<-1.3

pretty_palette <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA",
                    "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411",
                    "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788") #used for clustering plot

start<-proc.time()
set.seed(57)

# tuning functions (must be present in parallel environments)
#######


#cross elites - crosses 2 elite parents
cross_elites <-function(parent_models,mutate_threshold){
  #make parameter list
  xd<<-sample(parent_models$xdim,1,replace=T)
  yd<<-sample(parent_models$ydim,1,replace=T)
  rlen<<-sample(parent_models$rl,1,replace=T)
  alpha1<<- sample(parent_models$a1,1,replace=T)
  alpha2<<- sample(parent_models$a2,1,replace=T)
  radius<<-sample(parent_models$r,1,replace=T)
  
}


#mutate - crosses 2 elite parents and petrurbs random number of variables
mutate <-function(parent_models,mutate_threshold){
  
  #make parameter list
  xd=sample(parent_models$xdim,1,replace=T)
  yd=sample(parent_models$ydim,1,replace=T)
  rlen<-sample(parent_models$rl,1,replace=T)
  alpha1<- sample(parent_models$a1,1,replace=T)
  alpha2<- sample(parent_models$a2,1,replace=T)
  radius<-sample(parent_models$r,1,replace=T)
  
  #decide which paramaters to mutate
  mutate_xd=runif(1,0,1)
  mutate_yd=runif(1,0,1)
  mutate_rlen=runif(1,0,1)
  mutate_alpha1=runif(1,0,1)
  mutate_alpha2=runif(1,0,1)
  mutate_radius=runif(1,0,1)
  
  #mutate parameters which come in above mutate threshold = MAY WISH TO RECONSIDER DISTRIBUTION USED
  xd<<-ifelse(mutate_xd>=mutate_threshold,xd*rlnorm(1,0,1),xd)
  yd<<-ifelse(mutate_yd>=mutate_threshold,yd*rlnorm(1,0,1),yd)
  rlen<<-ifelse(mutate_rlen>=mutate_threshold,rlen*rlnorm(1,0,1),rlen)
  alpha1<<-ifelse(mutate_alpha1>=mutate_threshold,alpha1*rlnorm(1,0,1),alpha1)
  alpha2<<-ifelse(mutate_alpha2>=mutate_threshold,alpha2*rlnorm(1,0,1),alpha2)
  radius<<-ifelse(mutate_radius>=mutate_threshold,radius*rlnorm(1,0,1),radius)
}


#clamp parameters to ensure they stay in range
clamp_parameters<-function(xd,yd,radius,rlen,default_som_length,max_map_aspect_ratio,min_radius_recip,max_radius_recip,min_rlen,max_rlen){
  xd<<-ifelse(xd<default_som_length/max_map_aspect_ratio,default_som_length/max_map_aspect_ratio,xd)#min size for map is half of default - prevents maps shrinking too much betwen iterations
  yd<<-ifelse(yd<default_som_length/max_map_aspect_ratio,default_som_length/max_map_aspect_ratio,yd)
  
  xd<<-ifelse(xd>default_som_length*max_map_aspect_ratio,default_som_length*max_map_aspect_ratio,xd)#max size for map is twice default - prevents maps shrinking too much betwen iterations
  yd<<-ifelse(yd>default_som_length*max_map_aspect_ratio,default_som_length*max_map_aspect_ratio,yd)
  
  radius <<- ifelse ( radius < max(c(xd,yd))/min_radius_recip , max(c(xd,yd))/min_radius_recip, radius)
  radius<<- ifelse(radius>max(c(xd,yd))/max_radius_recip,max(c(xd,yd))/max_radius_recip,radius)
  
  rlen <<- ifelse ( rlen <min_rlen , min_rlen, rlen)
  rlen<<- ifelse(rlen>max_rlen,max_rlen,rlen)
}


#load in dataset
#######################################################

load("//nas01/analytics/Analytics Projects and Results/Segmentation using unsupervised learning/cc_data_for_segmentation.Rdata")

if(use_sample){
player_df <- sample_n(player_df,sample_size)
}
#######################################################



#process data and use non-spenders only
###########


non_spenders<-player_df %>%
  filter(num_iap_purchases==0)%>%
  select(max_day,active_days,max_session,gametime_hrs,furthest_level,coin_spent,
         num_level_attempts,num_unique_levels_attempted,avg_pass_rate,signed_in_to_Fb,num_event_rewards_won,max_event_reward_tier,
         num_distinct_coin_purchase_items,inactive_for_last_7_days,
         max_level_attempts,num_no_boost_attempts,num_no_boost_wins,coin_balance_range,num_stars_won,avg_stars_won_per_level,
         median_time_taken,has_decreasing_session_lengths,avg_session_length_mins)


non_spenders[is.na(non_spenders)]<-0
non_spenders<-scale(non_spenders)


bak_non_spenders<-non_spenders
non_spenders<-bak_non_spenders

default_som_length<-25# default is  round(sqrt(nrow(non_spenders)/20))


#random parameters for first iteration
#######

next_iteration_params <- data.frame()

for(p in 1:individuals_per_generation -elites_to_carry_forward ){
  xd<-sample((default_som_length/max_map_aspect_ratio):(default_som_length*max_map_aspect_ratio),1)
  yd<-sample((default_som_length/max_map_aspect_ratio):(default_som_length*max_map_aspect_ratio),1)
  rlen<-sample(c(min_rlen:max_rlen),1)
  alpha1<- rlnorm(1,0,1)*0.1
  alpha1<-ifelse(alpha1<0.5,alpha1,0.5)
  alpha2<- alpha1*runif(1,0.001,0.3)
  radius<-sample(max(c(xd,yd))/min_radius_recip:max(c(xd,yd))/max_radius_recip,1)
  s<-0
  
  parameter_list <- list(xd,yd,rlen,alpha1,alpha2,radius,s)
  next_iteration_params<-rbindlist(list(next_iteration_params,parameter_list))
}

#all_results_list<-list(rep(NULL,iterations)) #this will record the results for every individual for every generation (can be used for plotting later) - list of data tables

names(next_iteration_params)<-c("xdim","ydim","rl","a1","a2","r","S")





#create compute cluster
########


#cl<-makeCluster(detectCores())
cl<-makeCluster(6)
registerDoParallel(cl)



#evaluate initial population
#######


results <- foreach(j=1:nrow(next_iteration_params),.combine=rbind, .inorder = FALSE) %dopar%{
  
  require(kohonen)
  require(RColorBrewer)
  require(colorRamps)
  require(dplyr)
  require(cluster)
  require(foreach)
  
  
  xd<-next_iteration_params[j]$xdim
  yd<-next_iteration_params[j]$ydim
  rlen<-next_iteration_params[j]$rl
  alpha1<- next_iteration_params[j]$a1
  alpha2<- next_iteration_params[j]$a2
  radius<-next_iteration_params[j]$r
  
  
  som_grid <- somgrid(xdim = xd, ydim=yd, topo="hexagonal",toroidal=T)
  

  #generate SOM with the input parameters
  non_spenders.som <- som(non_spenders, 
                          grid=som_grid, 
                          rlen=rlen, #number of iterations
                          alpha=c(alpha1,alpha2), #learning rate
                          radius=radius, # default is 2/3 of map length
                          keep.data = TRUE)
  
  
  #find optimum number of clusters - calculates dissimilarity between groups - look for highest value of s
  s=NULL
  for(i in min_clusters:20){
    K=cutree(hclust(dist(non_spenders.som$codes[[1]]),method="ward.D"),i)
    s[i]= tryCatch({summary(silhouette(K,dist(non_spenders.som$codes[[1]])))$si.summary[3]}, error= function(){return(0)})
  }
  
  
  data.frame(xdim=xd,ydim=yd,rl=rlen,a1=alpha1,a2=alpha2,r=radius,S=max(s[min_clusters:length(s)]))
}


#sort initial population
results<-arrange(results,desc(S))
elites <- head(results,ceiling(nrow(next_iteration_params)/elitism_proportion))
next_iteration_params<-results
results<-NULL

print("initial population evaluated")




#iteratively tune paramaters
#######


for (k in (1: iterations)){
  
  results<-NULL
  
  #decide case for each individual and generate SOM
  ######
  
  results <- foreach(j=1:(individuals_per_generation -elites_to_carry_forward),
                     .export=c("cross_elites","mutate","clamp_parameters"),.combine=rbind, .inorder = FALSE,.verbose=T) %dopar%{


 #initialise variables and packages in parallel workers     
#######                    
    require(kohonen)
    require(RColorBrewer)
    require(colorRamps)
    require(dplyr)
    require(cluster)
    require(foreach)
           
                
    #variable assignment required due to use of superassignment operator in tuning functions                                   
     xd<-NULL
     yd<-NULL
     rlen<-NULL
     alpha1<- NULL
     alpha2<- NULL
     radius<-NULL
     min_radius_recip<-min_radius_recip #these variables are used onlt within functions so must be initialised in the worker workspace
     max_radius_recip<-max_radius_recip
     min_rlen<-min_rlen
     max_rlen<-max_rlen
     max_map_aspect_ratio<-max_map_aspect_ratio
     default_som_length<-default_som_length

     #compute SOMs on individual workers     
####### 
     
    #random number to choose course of action
    action<-runif(1,0,1)
    #choose two elites to cross
    parents <- elites[sample(nrow(elites), 2),]
    
    if(action<=cross_probability){  #cross case 
      
      cross_elites(parents)

    }else if(action<mutate_probability & action>=cross_probability){#mutate case
      
      mutate(parents,mutate_threshold)

    } else{#carry forward a cross of non-elite cases
      #choose two non elites to cross
      parents <- next_iteration_params[sample(nrow(next_iteration_params), 2),]
      mutate(parents,mutate_threshold)
    }
    
  
   # clamp_parameters(xd,yd,radius,rlen,default_som_length,max_map_aspect_ratio,min_radius_recip,max_radius_recip,min_rlen,max_rlen)
    xd<-ifelse(xd<default_som_length/max_map_aspect_ratio,default_som_length/max_map_aspect_ratio,xd)#min size for map is half of default - prevents maps shrinking too much betwen iterations
    yd<-ifelse(yd<default_som_length/max_map_aspect_ratio,default_som_length/max_map_aspect_ratio,yd)

    xd<-ifelse(xd>default_som_length*max_map_aspect_ratio,default_som_length*max_map_aspect_ratio,xd)#max size for map is twice default - prevents maps shrinking too much betwen iterations
    yd<-ifelse(yd>default_som_length*max_map_aspect_ratio,default_som_length*max_map_aspect_ratio,yd)

    radius <- ifelse ( radius < max(c(xd,yd))/min_radius_recip , max(c(xd,yd))/min_radius_recip, radius)
    radius<- ifelse(radius>max(c(xd,yd))/max_radius_recip,max(c(xd,yd))/max_radius_recip,radius)

    rlen <- ifelse ( rlen <min_rlen , min_rlen, rlen)
    rlen<- ifelse(rlen>max_rlen,max_rlen,rlen)

    #generate SOM with the input parameters
    som_grid <- somgrid(xdim = xd, ydim=yd, topo="hexagonal",toroidal=T)
    non_spenders.som <- som(non_spenders, 
                            grid=som_grid, 
                            rlen=rlen, #number of iterations
                            alpha=c(alpha1,alpha2), #learning rate
                            radius=radius, # default is 2/3 of map length
                            keep.data = TRUE)
    
    
    #find optimum number of clusters - calculates dissimilarity between groups - look for highest value of s
    s=NULL
    for(i in min_clusters:20){
      K=cutree(hclust(dist(non_spenders.som$codes[[1]]),method="ward.D"),i)
      s[i]= tryCatch({summary(silhouette(K,dist(non_spenders.som$codes[[1]])))$si.summary[3]}, error= function(){return(0)})
    }
    
    data.frame(xdim=xd,ydim=yd,rl=rlen,a1=alpha1,a2=alpha2,r=radius,S=max(s[min_clusters:length(s)]))
    
  }
  
  #####
  
  #add individuals to the list of all plots so can track progress later
  #plot_list_by_generation[length(plot_list_by_generation)+1]<-plot_list
  
  #carry forward the top individuals from last run
  for(c in(1:elites_to_carry_forward)){
    results<-rbind(results,elites[c,])
  }
  
  
  #get top individuals to carry forward to next generation - ie the new elites
  results<-arrange(results,desc(S))
  elites <- head(results,ceiling(nrow(next_iteration_params)/elitism_proportion))
  
  #all_results_list[length(all_results_list)+1]<-results #record the best parameters for this iteration
  next_iteration_params<-results
  
  
  print(k)
  print(elites[1,]$S)
  print(start-proc.time())
  
  write.xlsx(results,file=paste0("D:\\R code\\projects\\SOM\\parameters\\parameters",k,".xlsx")) #keep results in case of crash
  
  
}


write.xlsx(results,file="SOM_tuning_results_final.xlsx")

stopCluster(cl)
registerDoSEQ()

print(start-proc.time())


#######










# recreate winning SOM and plot

#aim for 10 instances per node

xdim<-elites[1,]$xdim
ydim<-elites[1,]$ydim
rlen<-10000#elites[1,]$rl
alpha1<-elites[1,]$a1
alpha2<-elites[1,]$a2
radius<-elites[1,]$r


som_grid <- somgrid(xdim = xdim, ydim=ydim, topo="hexagonal", toroidal = TRUE)

set.seed(57)
non_spenders.som <- som(non_spenders, 
                        grid=som_grid, 
                        rlen=rlen, #number of iterations
                        alpha=c(alpha1,alpha2), #learning rate
                        radius=radius, # default is 2/3 of map length
                        keep.data = TRUE)


s=NULL
for(i in 2:20){
  k=cutree(hclust(dist(non_spenders.som$codes[[1]]),method="ward.D"),i)
  s[i]= tryCatch({summary(silhouette(k,dist(non_spenders.som$codes[[1]])))$si.summary[3]}, error= function(){return(0)})
}


number_of_clusters<- which(s==max(s,na.rm=T))
number_of_clusters<- 18

#create heatplots for each attribute
for ( i in (1:length(dimnames(non_spenders)[[2]]))){
  # png(paste0("SOM plots/",dimnames(non_spenders)[[2]][i],".png"))
  plot(non_spenders.som, type = "property", property = non_spenders.som$codes[[1]][,i],
       palette.name = colorRamps::blue2red,ann=FALSE,yaxt='n',xaxt='n',main="")
  title(main=dimnames(non_spenders)[[2]][i],cex.main=2)
  # dev.off()
}



#plot clusters
som_cluster <- cutree(hclust(dist(non_spenders.som$codes[[1]])), number_of_clusters)

#png("D:\\R code\\projects\\SOM\\clusters.png")
plot(non_spenders.som, bgcol = pretty_palette[som_cluster], main = "Clusters",type="codes",shape="straight",codeRendering ="segments")
add.cluster.boundaries(non_spenders.som, som_cluster)
#dev.off()

#evaluate quality of SOM (look for flattening graph)
plot(non_spenders.som,type="changes")


#plot counts map (number of instances at each node)
plot(non_spenders.som, main = "BQ Data - node counts",type="count",shape="straight")

#plot node qualities (lower is better as it represents distance to codebook vectors)
plot(non_spenders.som, bgcol = pretty_palette[som_cluster], main = "quality",type="quality",shape="straight")


#view mapping of raw data
plot(non_spenders.som, bgcol = pretty_palette[som_cluster], main = "data mapping",type="mapping",shape="straight")

#plot U matrix
plot(non_spenders.som, type="dist.neighbours",main="U-matrix",shape="straight")














