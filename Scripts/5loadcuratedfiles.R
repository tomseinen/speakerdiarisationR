library(gtools)
library(rlist)
library(stringr)
library("PraatR")


#load all the file path names in the 3d matrix 
directorylist<-c("[directory1]",
                 "[directory2]")

# make 3d matrix, patient, session, number of parts, name and 4 other measures
noData <- rep(NaN, length(directorylist)*5*4); 
ar <- array(noData, c(length(directorylist), 5, 4));  


## Fill the ar with the paths of all wave datafiles
for(j in 1:length(directorylist)){
  print(j)
  files<-mixedsort(list.files(directorylist[j], pattern = "\\P.wav$"))
  filenrs<-unlist(lapply(files, function(x) (as.numeric(gsub("\\D", "",strsplit(x,split = "_")[[1]][2])))))
  
  cnt<-1
  cntin<-1
  for(i in 1:length(files)){
    if(i==1){
      ar[j,cnt,cntin]<-paste(directorylist[j],files[i],sep = "/")
      }
    else if(filenrs[i]==filenrs[i-1]){
      cntin<-cntin+1
      ar[j,cnt,cntin]<-paste(directorylist[j],files[i],sep = "/")
    }
    else {
      cntin<-1
      cnt<-cnt+1
      ar[j,cnt,cntin]<-paste(directorylist[j],files[i],sep = "/")
    }
  }
}



### data import
timesteppitch<-0.01
PitchArguments = list(timesteppitch, 75, 600)
FullPath = function(FileName){ return( paste( "C:/Users/Tom-Martijn/Documents/FortaOnderzoek/Forta4/", FileName, sep="") ) }

datalists<-NULL
for(p in (41:44)){ #Go over the patients dim(ar)[1]
  print(p)
  namses<-NULL
  for(s in 1:dim(ar)[2]){ #go over 5 sessions
    if (ar[p,s,1]!="NaN"){ # check if this session exists
      ext<-str_sub(ar[p,s,1],-9,-1)
      sessionClassPath<-sub(ar[p,s,1],pattern=ext,replacement=".MP3_classes.txt")
      sessionPitchPath<-sub(ar[p,s,1],pattern=ext,replacement=".PitchmatT")
      sessionMP3Path<-sub(ar[p,s,1],pattern=ext,replacement=".MP3")
      if (!file.exists(sessionClassPath)){print('error class file bestaat niet')}
      if (!file.exists(sessionPitchPath)){print('error pitch file bestaat niet')}
      if (!file.exists(sessionMP3Path)){print('error mp3 file bestaat niet')}
      
      # Get the classes that are patient
      classpaths<-ar[p,s,(ar[p,s,]!="NaN")] # get only the classpaths that exist
      classids<-as.integer(str_sub(classpaths,-7,-7)) # get the class ids, based on position!!
      #print(classids)
      
      # Load the classtable
      sessionclasses <- read.csv(sessionClassPath, sep=' ')
      # Load the pitch data
      SessionPitchData <- read.csv(sessionPitchPath, sep = ' ')
      
      # Gedoe om de begin time terug te krijgen, klein deel van mp3 nemen, daar pitch van maken en dan time van frame number ophalen.
      MP3Path<-paste( "C:/Users/Tom-Martijn/Documents/FortaOnderzoek/Forta4/", sessionMP3Path, sep="")
      tinyWavePath<-paste( "C:/Users/Tom-Martijn/Documents/FortaOnderzoek/Forta4/", sub(sessionMP3Path,pattern=".MP3",replacement="Tiny.wav"), sep="")
      tinyPitchPath<-paste( "C:/Users/Tom-Martijn/Documents/FortaOnderzoek/Forta4/", sub(sessionMP3Path,pattern=".MP3",replacement="Tiny.pitch"), sep="")
      #print(tinyPitchPath)
      if(!file.exists(tinyPitchPath)){
        print("Praat generates small pitch to get first time back...")
        praat("Extract part...",list(0, 2, "rectangular", 1, "yes"), input=MP3Path, output= tinyWavePath, overwrite=TRUE)
        PitchArguments = list(0.01, 75, 600)
        praat( "To Pitch...", arguments=PitchArguments, input=tinyWavePath, output=tinyPitchPath, overwrite=TRUE )
      }
      else{
        print("Small Pitch File Exists")
      }
      X1pitch<-praat( "Get time from frame number...", arguments=list(1),input=tinyPitchPath)
      Tx1<-as.numeric(strsplit(X1pitch," ")[[1]][1])
      SessionPitchData$t<-(as.numeric(rownames(SessionPitchData))*0.01)+(Tx1-0.01)
      
      TotalTimeSession<-praat( "Get total duration...", arguments=list(1),input=MP3Path)
      
      nameid<-strsplit(strsplit(ar[p,s,1],split = "/")[[1]][3],split = "_")[[1]][c(1,2)]
      
      namses<-c(namses,nameid[2])
      nam<-paste("p",nameid[1],sep="")
      
      for (rownr in 1:nrow(sessionclasses)) {
        if (sessionclasses$clusters[rownr] %in% classids){
          begintime<-sessionclasses$fvNoSil.t[rownr]
          endtime<-sessionclasses$fvNoSil.t[rownr+1]
          pitchpart<-SessionPitchData[(SessionPitchData$t>=begintime)&(SessionPitchData$t<endtime),]
          
          if (rownr==1){
            datalists[[nam]][[toString(s)]][1]<-pitchpart[1]
            datalists[[nam]][[toString(s)]][2]<-pitchpart[2]
            
          } else {
            datalists[[nam]][[toString(s)]][1]<-as.data.frame(c(unlist(datalists[[nam]][[toString(s)]][1]), unlist(pitchpart[1])))
            datalists[[nam]][[toString(s)]][2]<-as.data.frame(c(unlist(datalists[[nam]][[toString(s)]][2]), unlist(pitchpart[2])))
          }
        }
      }
      names(datalists[[nam]][[toString(s)]])<-c('p','t')
      names(datalists[[nam]])<-namses
    }
  }
}
    

AllPitchData<-datalists
save(AllPitchData, file = "Allpitchdata.rdata")
