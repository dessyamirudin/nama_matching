#measuring similarity between customer
#brief algorithm:
#similarity using Levenhstein algorithm.
#scale the distance into 0 to 1 by formula: (max.distance(a,b) - lev(a,b))/max.distance(a,b)
#take if name and address have similarity of >=0.8
#label using DBSCAN with 

#set working directory
setwd("~/folder")

#load library
library(tictoc)
library(stringr)
library(dbscan)

#reading data
customer = read.csv("sample_1000.csv",header=TRUE)

tic()
#calculating distance metrics
#deletingspace and dot from the file
customer$customer_name_clean =tolower(gsub("[[:space:]]|[.]|[,]", "", customer$customer_name))
customer$address_clean = tolower(gsub("[[:space:]]|[.]|[,]|[-]|[/]", "", customer$address))

#distance from customer clean using Levenhstein
dist.name = adist(customer$customer_name_clean,customer$customer_name_clean, partial = TRUE, ignore.case = TRUE)

#distance from address using Levenhstein
dist.address = adist(customer$address_clean,customer$address_clean, partial = TRUE, ignore.case = TRUE)

# scaling distance name
name.len = str_length(customer$customer_name_clean)
com.name = expand.grid(name.len,name.len)
com.name$sum = com.name$Var1+com.name$Var2
mat.name = matrix(com.name$sum,nrow(customer),nrow(customer))
scale.name = (mat.name-dist.name)/mat.name

# scaling distance address
address.len = str_length(customer$address_clean)
com.address = expand.grid(address.len,address.len)
com.address$sum = com.address$Var1+com.address$Var2
mat.address = matrix(com.address$sum,nrow(customer),nrow(customer))
scale.address=(mat.address-dist.address)/mat.address

# take only when similarity (scale distance) >0.8
scale.name.clean = ifelse(scale.name>=0.8,scale.name,0)
scale.address.clean = ifelse(scale.address>=0.8,scale.address,0)

scale.combine = (scale.name.clean+scale.address.clean)/2
scale.combine.clean = ifelse(scale.combine>=0.8,scale.combine,0)

# labeling by clustering the end result using DBSCAN
db.cluster = dbscan(scale.combine.clean,eps = 0.15,minPts = 1)

#assigning label to customer
customer$label = db.cluster$cluster
write.csv(customer,"customer_final.csv",row.names = FALSE)
toc()