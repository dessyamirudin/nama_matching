#measuring similarity between customer

#set working directory
# setwd("C:/Users/damiru01/OneDrive - Dentsu Aegis Network/Documents/Toyota/Practicing Name Matching")

#load library
library(tictoc)
library(stringr)
library(dbscan)

similar_text = function(input_file){
  #calculating distance metrics
  #deletingspace and dot from the file
  input_file =tolower(gsub("[[:space:]]|[.]|[,]", "", input_file))
  
  #distance from customer clean
  dist.input_file = adist(input_file,input_file, partial = TRUE, ignore.case = TRUE)
  
  # scaling distance name
  input_file.len = str_length(input_file)
  com.input_file = expand.grid(input_file.len,input_file.len)
  com.input_file$sum = com.input_file$Var1+com.input_file$Var2
  mat.input_file = matrix(com.input_file$sum,length(input_file),length(input_file))
  scale.input_file = (mat.input_file-dist.input_file)/mat.input_file
  
  # take only when similarity (scale distance) >0.8
  scale.input_file.clean = ifelse(scale.input_file>=0.8,scale.input_file,0)
  
  # labeling by clustering the end result using DBSCAN
  db.cluster = dbscan(scale.input_file.clean,eps = 0.15,minPts = 1)
  
  #assigning label to customer
  input_file = as.data.frame(input_file)
  input_file$label = db.cluster$cluster
  return(input_file)
}

#reading data
data_file = read.csv("file_text.csv",header=TRUE)
input_file = as.character(data_file[1:100,1])
final = similar_text(input_file)
write.csv(final,"final_similar.csv",row.names = FALSE)
