
library("stringr") ## needed for function str_length

complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        result<-data.frame(id=0,nobs=0)
        n=0
        for(i in id){
                n<-n+1
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
                
                result[n,"id"]<-i
                result[n,"nobs"]<-sum(complete.cases(x))
        }
        return(result)
}