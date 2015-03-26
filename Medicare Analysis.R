###init
setwd("C:/Users/Dell_PC/Desktop")

###Extracting a subset of the data using a SQLite connection
library(sqldf)
#Establishing a connection to SQLite database
conn<-dbConnect(SQLite(), dbname="C:/Users/Dell_PC/Desktop/clean.db")
#Running query to select relevant information
clusterData<-dbGetQuery(conn = conn, 
                        "SELECT description, 
                        avgSubmitted, 
                        avgAllowed, 
                        avgPayment, 
                        beneUnique,
                        srvc FROM clean 
                        WHERE state='IL'")
#Disconnecting
dbClearResult(clusterData)
dbDisconnect(conn)
detach(package:sqldf)

#Data cleanup and formatting
names(clusterData)<-c("description","submitted","allowed","payment","bene", "srvc")
clusterData$submitted<-as.numeric(clusterData$submitted)
clusterData$allowed<-as.numeric(clusterData$allowed)
clusterData$payment<-as.numeric(clusterData$payment)
clusterData$bene<-as.numeric(clusterData$bene)
clusterData$srvc<-as.numeric(clusterData$srvc)
clusterData$description<-as.factor(clusterData$description)

#Calculating discount and coverage parameters
clusterData<-cbind(clusterData,
                   1-clusterData$allowed/clusterData$submitted,
                   clusterData$payment/clusterData$allowed)
names(clusterData)<-c("description","submitted","allowed","payment","bene","srvc","discount","coverage")

#Standardizing parameters that will be used in clustering
clusterData$allowed<-(clusterData$allowed-min(clusterData$allowed))/(max(clusterData$allowed)-min(clusterData$allowed))
clusterData$payment<-(clusterData$payment-min(clusterData$payment))/(max(clusterData$payment)-min(clusterData$payment))
clusterData$bene<-(clusterData$bene-min(clusterData$bene))/(max(clusterData$bene)-min(clusterData$bene))
clusterData$srvc<-(clusterData$srvc-min(clusterData$srvc))/(max(clusterData$srvc)-min(clusterData$srvc))

#Medoid generation
library(sqldf)
medoids<-sqldf("SELECT * from clusterData limit 1")

for (i in 1:length(levels(clusterData$description))) {
  df<-clusterData[clusterData$description==levels(clusterData$description)[i],]
  if (nrow(df) == 1) {
    medoids[i,]<-df[1,]
  }
  else if (nrow(df) > 5000) {
    medoids[i,]<-df[clara(df[,c(5,6,7,8)],k=1,medoids.x = TRUE)$i.med,]
  }
  else {
    medoids[i,]<-df[pam(df[,c(5,6,7,8)],k=1)$id.med,]
  }
}

detach(package:sqldf)

#Clustering medoids of one state (IL)
wcss<-vector()

for (i in 2:30) {
  wcss[i-1] <- sum(kmeans(medoids[,c(5,6,7,8)], centers=i)$withinss)
}
plot(2:30,wcss,type="b", xlab="Number of Clusters", ylab="WCSS")
generatedClusters<-kmeans(medoids[,c(5,6,7,8)],centers=9)

library(rgl)
plot3d(medoids$discount,medoids$coverage,medoids$srvc, col = generatedClusters$cluster)
detach(package:rgl)


#Medoids for the state of Virginia
library(sqldf)
conn<-dbConnect(SQLite(), dbname="C:/Users/Dell_PC/Desktop/clean.db")

clusterData<-dbGetQuery(conn = conn, 
                        "SELECT description, 
                        avgSubmitted, 
                        avgAllowed, 
                        avgPayment, 
                        beneUnique,
                        srvc FROM clean 
                        WHERE state='VA'")

dbClearResult(clusterData)
dbDisconnect(conn)
detach(package:sqldf)

names(clusterData)<-c("description","submitted","allowed","payment","bene", "srvc")
clusterData$submitted<-as.numeric(clusterData$submitted)
clusterData$allowed<-as.numeric(clusterData$allowed)
clusterData$payment<-as.numeric(clusterData$payment)
clusterData$bene<-as.numeric(clusterData$bene)
clusterData$srvc<-as.numeric(clusterData$srvc)
clusterData$description<-as.factor(clusterData$description)
clusterData<-cbind(clusterData,
                   1-clusterData$allowed/clusterData$submitted,
                   (clusterData$payment)/clusterData$allowed)
names(clusterData)<-c("description","submitted","allowed","payment","bene","srvc","discount","coverage")
clusterData$allowed<-(clusterData$allowed-min(clusterData$allowed))/(max(clusterData$allowed)-min(clusterData$allowed))
clusterData$payment<-(clusterData$payment-min(clusterData$payment))/(max(clusterData$payment)-min(clusterData$payment))
clusterData$bene<-(clusterData$bene-min(clusterData$bene))/(max(clusterData$bene)-min(clusterData$bene))
clusterData$srvc<-(clusterData$srvc-min(clusterData$srvc))/(max(clusterData$srvc)-min(clusterData$srvc))

library(sqldf)
medoids2<-sqldf("SELECT * from clusterData limit 1")

for (i in 1:length(levels(clusterData$description))) {
  df<-clusterData[clusterData$description==levels(clusterData$description)[i],]
  if (nrow(df) == 1) {
    medoids2[i,]<-df[1,]
  }
  else if (nrow(df) > 5000) {
    medoids2[i,]<-df[clara(df[,c(5,6,7,8)],k=1,medoids.x = TRUE)$i.med,]
  }
  else {
    medoids2[i,]<-df[pam(df[,c(5,6,7,8)],k=1)$id.med,]
  }
}

detach(package:sqldf)

#Medoids for the state of California
library(sqldf)

#Establishing a connection to SQLite database
conn<-dbConnect(SQLite(), dbname="C:/Users/Dell_PC/Desktop/clean.db")

#Running query to select relevant information
clusterData<-dbGetQuery(conn = conn, 
                        "SELECT description, 
                        avgSubmitted, 
                        avgAllowed, 
                        avgPayment, 
                        beneUnique,
                        srvc FROM clean 
                        WHERE state='CA'")

#Disconnecting
dbClearResult(clusterData)
dbDisconnect(conn)
detach(package:sqldf)

#Data manipulation
names(clusterData)<-c("description","submitted","allowed","payment","bene", "srvc")
clusterData$submitted<-as.numeric(clusterData$submitted)
clusterData$allowed<-as.numeric(clusterData$allowed)
clusterData$payment<-as.numeric(clusterData$payment)
clusterData$bene<-as.numeric(clusterData$bene)
clusterData$srvc<-as.numeric(clusterData$srvc)
clusterData$description<-as.factor(clusterData$description)
clusterData<-cbind(clusterData,
                   1-clusterData$allowed/clusterData$submitted,
                   (clusterData$payment)/clusterData$allowed)
names(clusterData)<-c("description","submitted","allowed","payment","bene","srvc","discount","coverage")
clusterData$allowed<-(clusterData$allowed-min(clusterData$allowed))/(max(clusterData$allowed)-min(clusterData$allowed))
clusterData$payment<-(clusterData$payment-min(clusterData$payment))/(max(clusterData$payment)-min(clusterData$payment))
clusterData$bene<-(clusterData$bene-min(clusterData$bene))/(max(clusterData$bene)-min(clusterData$bene))
clusterData$srvc<-(clusterData$srvc-min(clusterData$srvc))/(max(clusterData$srvc)-min(clusterData$srvc))

library(sqldf)
medoids3<-sqldf("SELECT * from clusterData limit 1")

for (i in 1:length(levels(clusterData$description))) {
  df<-clusterData[clusterData$description==levels(clusterData$description)[i],]
  if (nrow(df) == 1) {
    medoids3[i,]<-df[1,]
  }
  else if (nrow(df) > 5000) {
    medoids3[i,]<-df[clara(df[,c(5,6,7,8)],k=1,medoids.x = TRUE)$i.med,]
  }
  else {
    medoids3[i,]<-df[pam(df[,c(5,6,7,8)],k=1)$id.med,]
  }
}
detach(package:sqldf)

#Medoids for the state of Michigan
library(sqldf)

#Establishing a connection to SQLite database
conn<-dbConnect(SQLite(), dbname="C:/Users/Dell_PC/Desktop/clean.db")

#Running query to select relevant information
clusterData<-dbGetQuery(conn = conn, 
                        "SELECT description, 
                        avgSubmitted, 
                        avgAllowed, 
                        avgPayment, 
                        beneUnique,
                        srvc FROM clean 
                        WHERE state='MI'")
#Disconnecting
dbClearResult(clusterData)
dbDisconnect(conn)
detach(package:sqldf)

#Data manipulation
names(clusterData)<-c("description","submitted","allowed","payment","bene", "srvc")
clusterData$submitted<-as.numeric(clusterData$submitted)
clusterData$allowed<-as.numeric(clusterData$allowed)
clusterData$payment<-as.numeric(clusterData$payment)
clusterData$bene<-as.numeric(clusterData$bene)
clusterData$srvc<-as.numeric(clusterData$srvc)
clusterData$description<-as.factor(clusterData$description)
clusterData<-cbind(clusterData,
                   1-clusterData$allowed/clusterData$submitted,
                   (clusterData$payment)/clusterData$allowed)
names(clusterData)<-c("description","submitted","allowed","payment","bene","srvc","discount","coverage")
clusterData$allowed<-(clusterData$allowed-min(clusterData$allowed))/(max(clusterData$allowed)-min(clusterData$allowed))
clusterData$payment<-(clusterData$payment-min(clusterData$payment))/(max(clusterData$payment)-min(clusterData$payment))
clusterData$bene<-(clusterData$bene-min(clusterData$bene))/(max(clusterData$bene)-min(clusterData$bene))
clusterData$srvc<-(clusterData$srvc-min(clusterData$srvc))/(max(clusterData$srvc)-min(clusterData$srvc))

library(sqldf)
medoids4<-sqldf("SELECT * from clusterData limit 1")
for (i in 1:length(levels(clusterData$description))) {
  df<-clusterData[clusterData$description==levels(clusterData$description)[i],]
  if (nrow(df) == 1) {
    medoids4[i,]<-df[1,]
  }
  else if (nrow(df) > 5000) {
    medoids4[i,]<-df[clara(df[,c(5,6,7,8)],k=1,medoids.x = TRUE)$i.med,]
  }
  else {
    medoids4[i,]<-df[pam(df[,c(5,6,7,8)],k=1)$id.med,]
  }
}
detach(package:sqldf)

#Medoids for the state of New York
library(sqldf)

#Establishing a connection to SQLite database
conn<-dbConnect(SQLite(), dbname="C:/Users/Dell_PC/Desktop/clean.db")

#Running query to select relevant information
clusterData<-dbGetQuery(conn = conn, 
                        "SELECT description, 
                        avgSubmitted, 
                        avgAllowed, 
                        avgPayment, 
                        beneUnique,
                        srvc FROM clean 
                        WHERE state='NY'")

#Disconnecting
dbClearResult(clusterData)
dbDisconnect(conn)
detach(package:sqldf)

#Data manipulation
names(clusterData)<-c("description","submitted","allowed","payment","bene", "srvc")
clusterData$submitted<-as.numeric(clusterData$submitted)
clusterData$allowed<-as.numeric(clusterData$allowed)
clusterData$payment<-as.numeric(clusterData$payment)
clusterData$bene<-as.numeric(clusterData$bene)
clusterData$srvc<-as.numeric(clusterData$srvc)
clusterData$description<-as.factor(clusterData$description)
clusterData<-cbind(clusterData,
                   1-clusterData$allowed/clusterData$submitted,
                   (clusterData$payment)/clusterData$allowed)
names(clusterData)<-c("description","submitted","allowed","payment","bene","srvc","discount","coverage")
clusterData$allowed<-(clusterData$allowed-min(clusterData$allowed))/(max(clusterData$allowed)-min(clusterData$allowed))
clusterData$payment<-(clusterData$payment-min(clusterData$payment))/(max(clusterData$payment)-min(clusterData$payment))
clusterData$bene<-(clusterData$bene-min(clusterData$bene))/(max(clusterData$bene)-min(clusterData$bene))
clusterData$srvc<-(clusterData$srvc-min(clusterData$srvc))/(max(clusterData$srvc)-min(clusterData$srvc))

library(sqldf)
medoids5<-sqldf("SELECT * from clusterData limit 1")
for (i in 1:length(levels(clusterData$description))) {
  df<-clusterData[clusterData$description==levels(clusterData$description)[i],]
  if (nrow(df) == 1) {
    medoids5[i,]<-df[1,]
  }
  else if (nrow(df) > 5000) {
    medoids5[i,]<-df[clara(df[,c(5,6,7,8)],k=1,medoids.x = TRUE)$i.med,]
  }
  else {
    medoids5[i,]<-df[pam(df[,c(5,6,7,8)],k=1)$id.med,]
  }
}

detach(package:sqldf)

#Adding state columns to medoids to identify
medoids<-cbind(medoids,'IL')
names(medoids)[9]<-'State'
medoids2<-cbind(medoids2,'VA')
names(medoids2)[9]<-'State'
medoids3<-cbind(medoids3,'CA')
names(medoids3)[9]<-'State'
medoids4<-cbind(medoids4,'MI')
names(medoids4)[9]<-'State'
medoids5<-cbind(medoids5,'NY')
names(medoids5)[9]<-'State'

#Clustering across multiple states
allMedoids<-rbind(medoids,medoids2,medoids3,medoids4,medoids5)
wcss<-vector()
for (i in 2:30) {
  wcss[i-1] <- sum(kmeans(allMedoids[,c(5,6,7,8)], centers=i)$withinss)
}
plot(2:30,wcss,type="b", 
  xlab="Number of Clusters", 
  ylab="WCSS", 
  main="Determining the Optimal Number of Clusters")
generatedClusters<-kmeans(allMedoids[,c(5,6,7,8)],centers=7)

#Cluster visualization
library(rgl)
plot3d(allMedoids$discount,allMedoids$coverage,allMedoids$srvc, 
  col = generatedClusters$cluster, 
  xlab = "Discount", 
  ylab = "Coverage", 
  zlab = "Number of Services",
  zlim=c(0,0.1))

plot3d(allMedoids$discount,allMedoids$coverage,allMedoids$bene, 
  col = generatedClusters$cluster, 
  xlab = "Discount", 
  ylab = "Coverage", 
  zlab = "Number of Beneficiaries")
detach(package:rgl)

#Finding the areas most needing Medicare attention
allMedoids<-cbind(allMedoids,generatedClusters$cluster)

#Note that the cluster number is hardcoded (allMedoids[,10]); the cluster number needs to be changed every time the code is re-run
improve<-allMedoids[(allMedoids[,7]<0.2 & allMedoids[,8]<0.8) & allMedoids[,10]==3,]

#Reformatting of HCPCS description
improve[,1]<-as.factor(as.character(improve[,1]))

#Finding the processes that require attention across the board (5 occurrences)
plot(improve[,1],
  xlab="Process",
  ylab="Number of occurrences".
  ,main="Processes requiring Medicare Attention")
sort(table(improve[,1]),decreasing=T)

#Finding processes from this cluster which occur in other clusters
library(sqldf)
totalOccurrences<-sqldf(
  "SELECT description, count(*) from allMedoids where description in (SELECT description FROM improve) GROUP by description")
clusterOccurrences<-sqldf(
  "SELECT description, count(*) from improve group by description")
detach(package:sqldf)

#Printing all processes which appear once in cluster 1 but 5 times overall
for (i in 1:nrow(totalOccurrences)) {
  if (totalOccurrences[i,2]==5 & clusterOccurrences[i,2]==1) {
    print(totalOccurrences[i,1])
    print(improve[improve$description==totalOccurrences[i,1],9])
    flush.console()
  }
}