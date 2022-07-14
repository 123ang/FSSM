library("XML")
library("methods")
library("stringr")

fssm_phase1 <- function(MaxVariable, xes_file,start,end){
  # Give the input file name to the function.
  result <- xmlParse(file = xes_file)
  # Exract the root node form the xml file.
  "rootnode" <- xmlRoot(result)
  var1 = "rn"
  assign(var1, rootnode)
  fulltable <- c(MaxVariable)
  structureTable <- c()
  for (j in  start:end) {
    var1 = c()
    child <- c()
    i = c()
    count = 1
    level = 1
    var1[level] = paste0("rn[[", j, "]]")
    assign(var1[level], rootnode)
    child[level] = xmlSize(eval(parse(text=var1[level])))
    i[level] = 1
    variable <- c("x0")
    FSSM <- c(xmlName(eval(parse(text=var1[level]))))
    
    colnum = 1
    
    while(!isTRUE(all.equal(child, 0))) {
      
      if(child[level] != 0) {
        child[level] = child[level] - 1 
        var1[level+1] <- paste0(var1[level],"[[",i[level],"]]")
        #next node
        level = level + 1
        colnum = colnum + 1
        if(is.null(xmlAttrs(eval(parse(text=var1[level])))[["value"]])) {
          FSSM <- cbind(FSSM, xmlName(eval(parse(text=var1[level]))))
        }
        else{
          var <- paste0(xmlAttrs(eval(parse(text=var1[level])))[[1]], "-" ,xmlAttrs(eval(parse(text=var1[level])))[[2]])
          FSSM <- cbind(FSSM, var)
          structureTable <- rbind(structureTable, xmlAttrs(eval(parse(text=var1[level])))[[1]])
        }
        
        variableCount = variableCount + 1
        variable <- cbind(variable,paste0("x",variableCount))
        child[level] <- xmlSize(eval(parse(text=var1[level])))
        if(level != 1) {
          i[level] = 1
        }
        else {
          i[level] <- i[level-1]
        }
        count = count + 1
      }
      
      if(child[level] == 0){
        i[level] = 1
        # reverse node
        level = level - 1
        if(level == 0){
          break
        }
        else {
          colnum = colnum + 1
          FSSM <- cbind(FSSM,'b')
          
        }
        if(child[level] != 0) {
          i[level] = i[level]+1
        }
        count = count + 1
      }
    }
    
    while(colnum < length(MaxVariable)) {
      colnum = colnum + 1
      FSSM <- cbind(FSSM,0)
    }
    
    fulltable <- rbind(fulltable, FSSM)
  }
  # get unique value so can create column name
  structureTable <- unique(structureTable)
  
  #fssm phase 1 flatten data, export into csv file
  write.csv(fulltable, file = "FSSM_phase1_flatten_data.csv")
  #unique values for columns name for structured table
  write.csv(structureTable, file = "structureTable_column_name.csv")
  
}

