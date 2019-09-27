library(tuneR)
library(mclust)
library(plotly)
library(matrixStats)
library(svMisc)
library(quantmod)
library(parallel)
library(pbapply)
library(dendextend)
library("PraatR")



# Setup:
#file<-"112541_s1_6-11-15"
#file<-"99649_s1_8-8-16.wav"
#file<-"113128_s1_18-2-16.wav"

directory<-"bestanden/112541-(compleet-16)"
directory<-"bestanden/113128-(compleet-16)"
directory<-"bestanden/113959-(compleet-16)"
directory<-"bestanden/114462-(compleet-16)"
directory<-"bestanden/115918-(compleet-16)"
directory<-"bestanden/116838-(compleet-16)"
directory<-"bestanden/117338-(compleet-16)"
directory<-"bestanden/117579-(compleet-16)"
directory<-"bestanden/117583-(compleet-16)"
directory<-"bestanden/117633-(compleet-16)"
directory<-"bestanden/118133-(compleet-16)"
directory<-"bestanden/118469-(compleet-16)"
directory<-"bestanden/118520-(compleet-16)"
directory<-"bestanden/118602-(compleet-16)"
directory<-"bestanden/99649"
directory<-"bestanden/113027"
directory<-"bestanden/113410"
directory<-"bestanden/117920"
directory<-"bestanden/119875"
directory<-"bestanden/112536-(compleet-15)"
directory<-"bestanden/120358-(compleet-15-s4-ontbreekt)"
directory<-"bestanden/100964-(compleet-16)"
directory<-"bestanden/99427"
directory<-"bestanden/112845" #test met 3
directory<-"bestanden/104908"
directory<-"bestanden/107671"
directory<-"bestanden/109206"
directory<-"bestanden/111556"
directory<-"bestanden/112654" 
directory<-"bestanden/113316"
directory<-"bestanden/113696" # laatste nog doen
directory<-"bestanden/114285" #
directory<-"bestanden/114319"
directory<-"bestanden/114392" #
directory<-"bestanden/114510" #
directory<-"bestanden/115385" #
directory<-"bestanden/116896" #
directory<-"bestanden/117162" #
directory<-"bestanden/117517" #
directory<-"bestanden/118217" #
directory<-"bestanden/119369" #16 al gedaan rest nog niet
directory<-"bestanden/120338" #
directory<-"bestanden/121008" #
directory<-"bestanden/121409" #


files<-list.files(directory, pattern = "\\.MP3$")
#if(length(files)==0)files<-list.files(directory, pattern = "\\.mp3$")
print(paste(length(files), "mp3 files"))
timestep<-0.01

start.time <- Sys.time()
for (x in files) {
  file<-x
  print(file)
  source("1import_praat_v2.R")
  source("2Segmentation_par.R")
  source("3clustering_par.R")
  source("4cutcombinewave_v3.R")
  remove(wave,wave_c1,wave_c2,wave_c3,wave_c4)
}
end.time <- Sys.time()
time.taken <- end.time - start.time 
time.taken

file<-files[1]
source("1import_praat_v2.R")
source("2Segmentation_par.R")
source("3clustering_par.R")

source("4cutcombinewave.R")
remove(wave,wave_c1,wave_c2,wave_c3,wave_c4)