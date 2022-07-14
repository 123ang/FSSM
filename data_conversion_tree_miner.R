filename = "flatten_data.csv"

data <- read.csv(filename ,header = FALSE)
data = data[1:200,1:40]
transform_data <- c()
unique_data <- unique(unlist(data))
transform_data <- c()
for(i in 1:length(unique_data)){
  z = i-1
  if(unique_data[i] != 'b'){
    transform_data <- rbind(transform_data,cbind(z,unique_data[i]))
  }
  else{
    transform_data <- rbind(transform_data,cbind('-1',unique_data[i]))
  }
}
colnames(transform_data) <- c("id","data")
transform_data <- as.data.frame(transform_data)
start_time <- Sys.time()


for(i in 1:nrow(data)) {
  for(j in 1:ncol(data)){
    for(z in 1:nrow(transform_data)){
      if(data[i,j] == transform_data[z,"data"]){
        data[i,j] = transform_data[z,"id"]
      }
    }
    
  }
}
end_time <- Sys.time()
time_diff <- end_time - start_time
new_data <- c()

for(i in 1:nrow(data)){
  columns <- 0
  for(j in 1:ncol(data)){
    columns <- columns + 1
  }
  new_data <- rbind(new_data,cbind(i,i,columns,data[i,]))
}  

write.table(new_data, paste0("zaki_fsm_",filename,".csv"), sep=" ",  col.names=FALSE, row.names = FALSE)
