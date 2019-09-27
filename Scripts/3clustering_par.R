#library(parallel)
#library(pbapply)

#IN: Feature vector, times
#OUT: clustered segments

# create bic diff matrix between all segments using 
# peaks
# fvNoSil, feature vector

print("clustering/")

#if(file.exists(paste(directory,"/",file,"_classes.txt", sep = ""))){
#  segments<-read.table(paste(directory,"/",file,"_classes.txt", sep = ""))
#  print("clustering already exists and loaded")
#  break
#}

# check number of cores and create cluster
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)

# predifined settings
cn<-(-c(1,14,15)) # columns used from the feature vector
#cn<-(-c(14,15)) # columns used from the feature vector
d<-dim(fvNoSil[,cn])[2] # parameter dimensions
labda<-1 # labda par for bic difference

# distance function: BIC difference, input=peak indices
d_funClust <- function(i, j) {
  x_i<-fvNoSil[which((fvNoSil$t>=peaks$fvNoSil.t[i]) & (fvNoSil$t<peaks$fvNoSil.t[i+1])),cn]
  x_j<-fvNoSil[which((fvNoSil$t>=peaks$fvNoSil.t[j]) & (fvNoSil$t<peaks$fvNoSil.t[j+1])),cn]
  x0<-rbind(x_i,x_j)
  cov_i<-cov(x_i)
  cov_j<-cov(x_j)
  cov0<-cov(x0)
  n_i<-nrow(x_i)#length segment i
  n_j<-nrow(x_j)
  n0<-n_i+n_j #length sum two segments
  bicdif<-(n0/2)*log(det(cov0))-(n_i/2)*log(det(cov_i))-(n_j/2)*log(det(cov_j))-(1/2)*(labda)*(d+(1/2)*d*(d+1))*log(n0)
  bicdif
}

#Send variables to cluster
clusterExport(cl, c("d_funClust","peaks","cn","fvNoSil","d","labda"))


#create distance matrix using parallel execution
D_1 <- 
  pbsapply(1:nrow(peaks),cl = cl, function(i) #parSapply with process bar =pbsapply
    sapply(i:nrow(peaks), function(j) d_funClust(i, j))
  )
clusterExport(cl,c("D_1"))
D_2<-pbsapply(1:(length(D_1)-1), cl = cl, function(i)
  (c(rep(0,i),unlist(D_1[i+1])))
)
D_2<-t(cbind(unlist(D_1[1]),D_2))
stopCluster(cl)
D_2[lower.tri(D_2)] <- t(D_2)[lower.tri(D_2)]

b<-D_2
b[is.na(b)] <- 0 # set all na to 0
b<-b*-1 # Swap minus sign, make the largest score the smallest distance
di <- as.dist(b) # from matrix to distance object
hc<- hclust(di, method = "ward.D2") # cluster
clusters <- cutree(hc, 4) # cut at 3 clusters

segments<-cbind(peaks,clusters)
#save segments for later use
write.table(segments, file=paste(directory,"/",file,"_classes.txt", sep = ""))

dend<-as.dendrogram(hc)
colors_to_use <- clusters[order.dendrogram(dend)]
labels_colors(dend) <- colors_to_use+1
plot(dend, main=file)

DM = as.matrix(as.dist(b))
HC = hclust(as.dist(DM), method="ward.D2")
plot(2:20, sapply(2:20, function(i) { 
  mean(cluster::silhouette(cutree(HC, i), dmatrix=DM)[,"sil_width"]) }),
  xlab="Number of clusters", ylab="Average Silhouette", type="b", pch=20, main=file)

#plot(fvNoSil$t,fvNoSil$p,xlim=c(1000,1150), ylim=c(-10,600))
#points(segments$fvNoSil.t,fvNoSil$p[match(segments$fvNoSil.t,fvNoSil$t)], col=segments$clusters+1, pch=18)

#plot(fvNoSil$t,fvNoSil$p,xlim=c(1065,1067), ylim=c(-10,600))
#points(segments$fvNoSil.t,fvNoSil$p[match(segments$fvNoSil.t,fvNoSil$t)], col=segments$clusters+1, pch=18)
#points(fv$t,fv$sil,col=fv$sil+1)
