#library(svMisc)
#library(quantmod)
#library(parallel)
#library(pbapply)

#IN: Feature vector
#OUT: Times of change points

print("segmentation/")

# check number of cores and create cluster
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)

#set the window size, w being the number of feature vectors in one window.
w<-50

# column numbers of the feature vector used
#cn<-c(1,2)
cn<--c(1,14,15)
#cn<--c(14,15)
d<-dim(fvNoSil[,cn])[2]
labda<-1

d_funSeg <- function(obs) {
  cov0<-cov(fvNoSil[c((obs-(w-1)):(obs+w)),cn])
  cov1<-cov(fvNoSil[c((obs-(w-1)):(obs)),cn])
  cov2<-cov(fvNoSil[c((obs+1):(obs+w)),cn])
  n0<-w*2
  n1<-w
  n2<-w
  bicdif<-(n0/2)*log(det(cov0))-(n1/2)*log(det(cov1))-(n2/2)*log(det(cov2))-(1/2)*(labda)*(d+(1/2)*d*(d+1))*log(n0)
  bicdif
}

clusterExport(cl, c("d_funSeg","cn","fvNoSil","d","labda","w"))

bd<-pbsapply(w:(dim(fvNoSil)[1]-w),cl=cl, function(obs) d_funSeg(obs))
stopCluster(cl)

bicdif<-c(rep(0,49),bd,rep(0,50))

# convert to dataframe and add time and samplenumber
bicdif<-data.frame(bicdif, fvNoSil$t, fvNoSil$sample)

# plot the bic diferences
plot_ly(data = bicdif, x = ~fvNoSil.t, y = ~bicdif)

# find the changepoints
# using growing window detection
find_peaks <- function (x, m = 3, min=0, distance=0){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks <- pks[which(x[pks]>min)] # remove points under the min threshold
  pks <- pks[which(c(pks[1],diff(pks))>distance)]
  pks
}

# peak location in bicdif matrix, at least 150 points ascending, 75 min threshold value, and min distance between peaks is 100 points.
peaklocations<-find_peaks(bicdif$bicdif, m = 100, min = 50 , distance = 100)

#plot(bicdif$bicdif, xlim=c(3000,3500), ylim=c(-100,400))
#points(peaklocations,bicdif[peaklocations,'bicdif'], col='green',pch=18)

#plot(bicdif$fvNoSil.t,bicdif$bicdif, xlim=c(200,700), ylim=c(-100,400))
#points(bicdif[peaklocations,'fvNoSil.t'],bicdif[peaklocations,'bicdif'], col='green',pch=18)

# create peak times and samplelocations in original feature vector.
peaks<-bicdif[peaklocations,c("fvNoSil.t","fvNoSil.sample")]
print(nrow(peaks))
### peak times ready, continue with clustering