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

## Setup:
# specify directory with audio file to be diarized
directory<-"[Directory]"

# get MP3 files, or change extension
files<-list.files(directory, pattern = "\\.MP3$")
print(paste(length(files), "mp3 files"))
timestep<-0.01

# Go over files
start.time <- Sys.time()
for (x in files) {
  file<-x
  print(file)
  source("1import_praat.R")
  source("2Segmentation_par.R")
  source("3clustering_par.R")
  source("4cutcombinewave.R")
  remove(wave,wave_c1,wave_c2,wave_c3,wave_c4)
}
end.time <- Sys.time()
time.taken <- end.time - start.time 
time.taken

remove(wave,wave_c1,wave_c2,wave_c3,wave_c4)