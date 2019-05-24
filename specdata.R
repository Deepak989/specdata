pollutantmean<- function(directory,pollutant,id= 1:332){
        file_list<-list.files(directory)
        pollutant_val<- vector()
        for(i in id){
                data<-read.csv(file_list[i]) 
                #getting the data frame from csv file
                pollutant_val<-c(pollutant_val,data[, pollutant])
        }
        
        mean(pollutant_val, na.rm= TRUE)
        
        
}


complete<- function(directory,id=1:332){
        file_list<-list.files(directory)
        ids<-vector()
        complete_cases<-vector()
        for(i in id){
                data<-read.csv(file_list[i]) 
                ids<-c(ids,i)
                complete_cases<-c(complete_cases,sum(complete.cases(data)))
        }
        data.frame(ids,complete_cases)
        
        
}


corr_pollutants<-function(directory,threshold=0){
        file_list<-list.files(directory)
        df<-complete(directory)
        file_compl<-file_list[df$complete_cases>threshold]
        corr_poll<-vector()
        for(f in file_compl){
                if(!grepl("csv",f,fixed = TRUE)){
                        break()
                }
                
                data<-read.csv(f)
                compdf<-data[complete.cases(data),]
                corr_poll<- c(corr_poll,cor(compdf$nitrate,compdf$sulfate))
        }
        
        corr_poll
        
}
        
