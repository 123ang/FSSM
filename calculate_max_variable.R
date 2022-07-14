# Load the package required to read XML files.
# install.packages("XML")
# install.packages("stringr")
library("XML")
library("methods")
library("stringr")


calculate_max_variable() <- function(xes_file,start,end){
  # Give the input file name to the function.
  result <- xmlParse(file = xes_file)
  # Exract the root node form the xml file.
  "rootnode" <- xmlRoot(result)
  #calculate which tree is the longest chain and get the longest tree
  
  MaxT <- c()
  MaxTCount <- 0
  MaxVariable <- c()
  var1 = "rn"
  assign(var1, rootnode)
  bc <- c()
  tc <- c()
  vc <- c()
  
  #end = 235

  for (j in start:end) {
    var1 = c()
    child <- c()
    i = c()
    count = 1
    level = 1
    variableCount = 1
    var1[level] = paste0("rn[[", j, "]]")
    assign(var1[level], rootnode)
    child[level] = xmlSize(eval(parse(text=var1[level])))
    i[level] = 1
    variable <- c("x0")
    while(!isTRUE(all.equal(child, 0))) {
      if(child[level] != 0) {
        child[level] = child[level] - 1 
        var1[level+1] <- paste0(var1[level],"[[",i[level],"]]")
        level = level + 1
        variable <- cbind(variable,paste0("x",variableCount))
        variableCount = variableCount + 1
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
        level = level - 1
        if(level == 0){
          break
        }
        else {
          variable <- cbind(variable,paste0("x",variableCount))
          variableCount = variableCount + 1
        }
        if(child[level] != 0) {
          i[level] = i[level]+1
        }
        count = count + 1
      }
    }
    
    if( variableCount > MaxTCount) {
      MaxT <- var1[1]
      MaxTCount = variableCount
      MaxVariable = variable
    }
  }
  MaxVariable
}

