
library("XML")
library("methods")
library("stringr")
fssm_phase2 <- function(){

  structureTable = read.csv("structureTable_column_name.csv")
  structureTable <- as.data.frame(structureTable)
  structureTable <- t(structureTable)
  structureTable <- tail(structureTable, -1)
  #flatten dataset
  fulltable = read.csv("FSSM_phase1_flatten_data.csv")
  
  tempStruc <- c()
  
  #column names
  colVar <- structureTable 
  fullStruc <-  t(c("transaction_no", colVar))
  
  nodeLevel = 0
  tempvar <- c()
  fullStructureTable <- c()
  
  
  
  for(i in start:end){
    nodeLevel = 0
    fullStruc <- c()
    for(j in 1:ncol(fulltable)){
      if(fulltable[i,j] == 'b'){
        nodeLevel = nodeLevel - 1
      }
      else{
        if(fulltable[i,j] != '0'){
          nodeLevel = nodeLevel + 1
        }
      } 
      if(nodeLevel > 1) {
        if(str_detect(fulltable[i,j],"-")){
          var <- strsplit(fulltable[i,j], "-")
          var <- matrix(unlist(var), ncol=1, byrow=TRUE)
          for(z in 1:length(colVar)){
            tempvar <- c()
            if(colVar[z] == var[1]){
              if(length(var)>2){
                varCount = 2
                while(varCount <= length(var)){
                  if(varCount == 2){
                    tempvar <- var[varCount]
                  }
                  else{
                    tempvar <- paste0(tempvar,"-", var[varCount])
                  }
                  varCount = varCount + 1
                }
                tempStruc[z] <- tempvar
              }
              else{
                tempStruc[z] <- var[2]
              }
            }
          }
        }
      }
      if(length(tempStruc)>0){
        if(nodeLevel == 1) {
          a <- 1
          while(a <= length(structureTable)){
            if(is.na(tempStruc[a])){
              tempStruc[a] = 0
            }
            a <- a + 1
          }
          tempStruc <- append(paste0("T",i-1), tempStruc)
          fullStruc = rbind(fullStruc, tempStruc)
          tempStruc <- c()
        }
      }
    }
    
    fullStructureTable <- rbind(fullStructureTable,fullStruc)
    print(paste0("Task complete for ", i-1))
  }
  colnames(fullStructureTable) <- c('Transactions',structureTable)
  # export structured data
  write.csv(fullStructureTable, file = "fullStructureTable.csv")
}
