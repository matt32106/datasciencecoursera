
library("stringr") ## needed for function str_length

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        comp<-complete(directory) ## which files are complete
        
        ok<-comp[comp["nobs"]>threshold,] ## how many observations usable
        toprocess<-ok[,"id"]
print(c("to process : ",length(toprocess)))       
        dn <- vector()
        ds <- vector()
        result<-numeric(0)
        
        for(i in toprocess) {
                a<-i
                ## complete the name of the file with leading 0s
                if (str_length(a[1]) == 1) {
                        a<-paste("00",a,sep="")
                } 
                else if (str_length(a[1]) == 2) {
                        a<-paste("0",a,sep="")
                }
                sFile<-paste(directory,"/",a,".csv",sep="")
                
                x <- read.csv(file=sFile,header = TRUE)
                test <- complete.cases(x)
                vData <- x[test,]
                dn<-vData[,"nitrate"]
                ds<-vData[,"sulfate"]

                result<-c(result,cor(ds,dn))
        }
##        print(dn)
##        print(ds)

        return (result)
}