
req_pkgs = c('dplyr', 'ggplot2')
lapply(req_pkgs, require, character.only = TRUE)
options(repr.plot.width=10, repr.plot.height=8)
options(scipen = 999)

data <- read.csv("Auto_Insurance_Claims_Sample.csv",header = TRUE)

data %>% head

str(data)

dim(data)

# 2 is to represent that we are finding unique values along the columns 
apply(data, 2, function(x) c(sum(is.na(x)),length(unique(x))))

colNames <- names(data)
catCols <- c()
numCols <-c()
dtypeList <- lapply(data, function(x) class(x))
  for (cols in colNames){
    if (dtypeList[[cols]]=="factor" | dtypeList[[cols]]=="character")
    {
      catCols <-c(catCols, cols)
    }
   else if(length(unique(data[[cols]])) <= 30)
   {
      catCols <-c(catCols, cols)
   }
    else{
      numCols <-c(numCols, cols)
    }
  }

catCols

catSummary<- function(data, varName){
  
 # Params

 # data - The data frame 
 # varName- Name of the categorical variable



  
  #varName <- "Education"
  tempdata<- count(data, data[[varName]])
  numLevels <- length(unique(data[varName]))
  tempdata <- cbind(c(rep(varName, numLevels)),tempdata)
  names(tempdata)[1] <- "VariableName"
  names(tempdata)[2] <- "Levels"
  names(tempdata)[3]<- "Frequency"
  tempdata<- tempdata[order(-tempdata["Frequency"]),]
  tempdata <- cbind(tempdata,(tempdata["Frequency"]/sum(tempdata["Frequency"]))*100)
  names(tempdata)[4]<- "Percent"
  tempdata <- cbind(tempdata, cumsum(tempdata["Frequency"]))
  names(tempdata)[5] <- "CumulativeFrequency"
  tempdata <- cbind(tempdata, cumsum(tempdata["Percent"]))
  names(tempdata)[6]<- "CumulativePercent"
    
    
  #tempdata["VariableName"] <-NA
  #tempdata[1, "VariableName"] <- varName
  
  return (tempdata)  }

numSummary<- function(data, varName){
  
  N_non_nulls = nrow(data[!is.na(data[varName]),])
  N_nulls     =nrow(data[is.na(data[varName]),])
  max_ <- max(data[[varName]], na.rm=TRUE)
  min_ <-min(data[[varName]], na.rm=TRUE)
  mean_ <-mean(data[[varName]], na.rm=TRUE)
  summary <- quantile(data[[varName]], probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99), na.rm=TRUE)
  full_summary<- c(varName,N_non_nulls, N_nulls,max_,min_,mean_,summary)
  
  return (full_summary)
}

 numDF<- data.frame(matrix(ncol=15, nrow=length(numCols)))
  colnames(numDF)<-c("Variable","N","N Miss","Maximum","Minimum","Mean","p1","p5","p10","Q1","50thPctl","Q3","p90","p95","p99")
  
  
  for (i in 1:length(numCols))
  {
  numDF[i,]<- numSummary(data, numCols[i])
}


plotDataFunction<- function(data, varName, depVarName,categorical= TRUE, bins =10 ){
  
  if (categorical){
  
  temp_summary_df  <- data %>% group_by_at(varName) %>% summarise_at(.vars = depVarName,funs(mean_ = mean(., na.rm = TRUE) , s1=mean(., na.rm = TRUE) - sd(., na.rm = TRUE), s2=mean(., na.rm = TRUE) + sd(., na.rm = TRUE)))
  
  tempDF <- count(data, data[[varName]])
  names(tempDF)[1]<- varName
  names(tempDF)[2]<-"Frequency"
  tempDF <- tempDF[order(tempDF[[varName]]),]
  tempDF<- merge(tempDF, temp_summary_df, by = varName)
  names(tempDF)[3]<- "depVarMean"
  names(tempDF)[4]<-"mean-std"
  names(tempDF)[5]<-"mean+std"  
  }
    
  else 
  {
  
  intervalVector<-(cut(data[[varName]], breaks=c(quantile(data[[varName]],probs = seq(0,1,1/bins),na.rm=TRUE)), include.lowest=TRUE, dig.lab= 5))  
  
  data_append_intervalVector<- cbind(data,newVar = intervalVector)
 
  
  temp_summary_df  <- data_append_intervalVector %>% group_by(newVar) %>% summarise_at(.vars = depVarName,funs(mean_ = mean(., na.rm = TRUE) , s1=mean(., na.rm = TRUE) - sd(., na.rm = TRUE), s2=mean(., na.rm = TRUE) + sd(., na.rm = TRUE)))
  names(temp_summary_df)[1]<- varName
  

  
  tempDF <- dplyr::count(data_append_intervalVector, newVar)
   names(tempDF)[1]<- varName
  names(tempDF)[2]<-"Frequency"
  tempDF <- tempDF[order(tempDF[[varName]]),]
  tempDF<- merge(tempDF, temp_summary_df, by = varName)
  
 
  names(tempDF)[3]<- "depVarMean"
  names(tempDF)[4]<-"mean-std"
  names(tempDF)[5]<-"mean+std"  
  tempDF<-tempDF[order(tempDF[[varName]]),]    
    
  }
  return(tempDF)
    
  
}
    
  

#plotDataFunction(data, "Claim.Amount", "Total.Claim.Amount", FALSE)

# Exporting summaries for some variables
                    
write.csv(numDF, "numericSummary.csv")
write.csv(catSummary(data, "Education"), "catSummary.csv")

write.csv(plotDataFunction(data, "Claim.Amount", "Total.Claim.Amount",FALSE), "numericLevelWise.csv")

write.csv( plotDataFunction(data, "Education", "Total.Claim.Amount",TRUE), "catLevelWise.csv")
