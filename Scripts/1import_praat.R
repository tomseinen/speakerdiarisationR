print("importing files/")

##path of wave file
# specify your project directory C:/Users/../Documents/Rproject/
# needed for PraatR
FullPath = function(FileName){ return( paste( "[Fullpath to your project directory]", FileName, sep="") ) }

#Get mp3path
MP3path = FullPath(paste(directory,file,sep="/"))
file.exists(MP3path)

## Use praat to create a pitch file
print("Pitch")
PitchPath = sub(MP3path,pattern=".MP3",replacement=".Pitch")
PitchPathMat = sub(MP3path,pattern=".MP3",replacement=".Pitchmat")
PitchPathMatT = sub(MP3path,pattern=".MP3",replacement=".PitchmatT")
if(!file.exists(PitchPath)){
  PitchArguments = list(timestep, 75, 600)
  praat( "To Pitch...", arguments=PitchArguments, input=MP3path, output=PitchPath, overwrite=TRUE )
  X1pitch<-praat( "Get time from frame number...", arguments=list(1),input=PitchPath)
  praat( "To Matrix", input=PitchPath, output=PitchPathMat, overwrite=TRUE)
  praat( "Transpose", input=PitchPathMat, output=PitchPathMatT, overwrite=TRUE, filetype="headerless spreadsheet" )
} else{
  print("Pitch exists")
  X1pitch<-praat( "Get time from frame number...", arguments=list(1),input=PitchPath)
}

## Use praat to create MFCC data
print("MFCC")
MFCCPath = sub(MP3path,pattern=".MP3",replacement=".MFCC")
MFCCPathMat = sub(MP3path,pattern=".MP3",replacement=".MFCCmat")
MFCCPathMatT = sub(MP3path,pattern=".MP3",replacement=".MFCCmatT")
if(!file.exists(MFCCPath)){
  MFCCArguments = list(12,0.02,timestep,100.0,100.0,0.0)
  praat( "To MFCC...", arguments=MFCCArguments, input=MP3path, output=MFCCPath, overwrite=TRUE)
  X1mfcc<-praat( "Get time from frame number...", arguments=list(1),input=MFCCPath)
  praat( "To Matrix", input=MFCCPath, output=MFCCPathMat, overwrite=TRUE)
  praat( "Transpose", input=MFCCPathMat, output=MFCCPathMatT, overwrite=TRUE, filetype="headerless spreadsheet" )
} else{
  print("MFCC exists")
  X1mfcc<-praat( "Get time from frame number...", arguments=list(1),input=MFCCPath)
}

# Remove the files again to reduce a lot of space!
file.remove(PitchPath)
file.remove(MFCCPath)
file.remove(PitchPathMat)
file.remove(MFCCPathMat)
remove(PitchPath,MFCCPath,PitchPathMat,MFCCPathMat)

#Import the pitch and mfcc data
print("Import")
MFCCData<-read.table(MFCCPathMatT)
PitchData<-read.table(PitchPathMatT)

#get time first frame
if(X1mfcc==X1pitch){Tx1<-as.numeric(strsplit(X1pitch," ")[[1]][1])
} else {print("ERROR: x1 not the same")}

#combine data and add names and time
fv<-cbind(PitchData, MFCCData)
colnames(fv)<-c("p","mfcc1","mfcc2","mfcc3","mfcc4","mfcc5","mfcc6","mfcc7","mfcc8","mfcc9","mfcc10","mfcc11","mfcc12")
fv$t<-(as.numeric(rownames(fv))*timestep)+(Tx1-timestep)

fvNoSil<-fv
fvNoSil$sample<-as.numeric(rownames(fvNoSil))
fvNoSil<-fvNoSil[-which(fvNoSil$p == 0), ]

#Get silences
silence<-rep(0,nrow(fv))
cnt<-0
strt<-NULL
minimalsilence<-100 #=1 sec
for (i in 2:nrow(fv)) {
  if(fv$p[i]==0){
    if(fv$p[i-1]!=0){
      cnt<-1
      strt<-i
    }
    else if(fv$p[i-1]==0){
      cnt<-cnt+1
    }
  }
  if(fv$p[i]!=0 & fv$p[i-1]==0){
    if(cnt>minimalsilence){
      silence[strt]<-1 # set 1 as start
      silence[i-1]<-2 # set 2 as end
    }
  }
}
silence[nrow(fv)]<-2
fv$sil<-silence
silstrt<-data.frame(fv[which(fv$sil>0),c("t","sil")])
silstrt$clusters<-0 # add empty clusters column
silstrt<-silstrt[,c(1,3,2)] # reorder columns

# out: fv (Features vector)
