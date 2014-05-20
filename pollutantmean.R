## TESTING DATA
## setwd("~/GitHub/datasciencecoursera")
## source("pollutantmean.R")
## pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
## pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
## pollutantmean("specdata", "nitrate", 23)
## [1] 1.281

library("stringr") ## needed for function str_length

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        vDataSum = NULL ## vector will be used to append  
                        ## values from the different CSVs
        for(i in id){
                a<-i
                ## complete the name of the file with leading 0s
                if (str_length(a[1]) == 1) {
                        a<-paste("00",a,sep="")
                } 
                else if (str_length(a[1]) == 2) {
                        a<-paste("0",a,sep="")
                }
                sFile<-paste(directory,"/",a,".csv",sep="")
                ##print(sFile)
                      
                x <- read.csv(file=sFile,header = TRUE)##, nrows = 50)
                x <- x[pollutant]
                vData <- x[!is.na(x)]
                
                vDataSum<-append(vDataSum,vData) 
        } ## finished reading files, vDataSum contains all data needed
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        nResult <- round(mean(vDataSum),3)
        
        return(nResult)
}