#library(tuneR)
#library(svMisc)

#IN: times, wave
#OUT: cluster seperated waves

print("Cut and combine/")

# Import wave file using tuneR
print("reading wave file")
wave<-readMP3(paste(directory,file,sep="/"))

#extract wave objects
#extractWave()
#combine wave objects
#bind(object, ...)
#save
#writeWave(object, filename, extensible = TRUE)

segments<-segments[,-c(2)]
segments$sil<-0
colnames(segments)<-c("t","clusters","sil")
segClSil<-rbind(segments,silstrt)
segClSil<-segClSil[order(segClSil$t),] 

#create wave objects for the clusters
for(c_index in 1:max(segClSil$clusters)) {
  assign(paste("wave_c", c_index, sep = ""), extractWave(wave,from = 0,to = 0.001,xunit = "time"))
}
#create wave objects for the clusters
for(c_index in 1:max(segClSil$clusters)) {
  assign(paste("times_c", c_index, sep = ""), data.frame(timestart=integer(),length=integer()))
}
#fill the wave objects with the segments, without the silences longer than 0.5 sec
currentcl<-0
for (row in 1:(nrow(segClSil)-1)) {
  if(segClSil$clusters[row]>0){
    currentcl<-segClSil$clusters[row]
    #cut piece from wave & paste to new wave
    ex<-extractWave(wave,from = segClSil$t[row],to = segClSil$t[row+1],xunit = "time")
    assign(paste("times_c", currentcl, sep = ""), rbind(get(paste("times_c", currentcl, sep = "")),c(segClSil$t[row],length(ex))))
    assign(paste("wave_c", currentcl, sep = ""), bind(get(paste("wave_c", currentcl, sep = "")),ex))
  }
  if(segClSil$clusters[row]==0 & currentcl!=0){
    if(segClSil$sil[row]==2){
      ex<-extractWave(wave,from = segClSil$t[row],to = segClSil$t[row+1],xunit = "time")
      assign(paste("times_c", currentcl, sep = ""), rbind(get(paste("times_c", currentcl, sep = "")),c(segClSil$t[row],length(ex))))
      assign(paste("wave_c", currentcl, sep = ""), bind(get(paste("wave_c", currentcl, sep = "")),ex))
    }
  }
}

print("writing files")
writeWave(wave_c1,sub(paste(directory,file,sep="/"),pattern = ".MP3", replacement = "_c1n.wav"), extensible = F)
writeWave(wave_c2,sub(paste(directory,file,sep="/"),pattern = ".MP3", replacement = "_c2n.wav"), extensible = F)
writeWave(wave_c3,sub(paste(directory,file,sep="/"),pattern = ".MP3", replacement = "_c3n.wav"), extensible = F)
writeWave(wave_c4,sub(paste(directory,file,sep="/"),pattern = ".MP3", replacement = "_c4n.wav"), extensible = F)

