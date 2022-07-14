data<- read.csv('fullStructureTable.csv')
data <- data[,2:5]
j = 2
filter_data<- c()
temp <- c()
for(i in 1:nrow(data)) {
  transaction <- gsub("T","",data[i,1])
  if(data[i,4] != 0) {
    
    temp <- cbind(j,data[i,])
    j=j+1
    
    filter_data<- rbind(filter_data,temp)
    temp <-c()
    
  }
 

  
}


filter_data<- rbind(filter_data,temp)