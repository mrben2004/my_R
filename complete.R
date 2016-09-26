complete <- function(directory, id=1:331){
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a dat frame of the form:
        ## id nobs
        ## 1   117
        ## 2   1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the 
        ## number of complete cases
        ## if the id beyond or below the scope defined above the return with NA
        if (id[1] < 1 | id[length(id)]>332 ) {
                return(NA)
        }
        # append the working directory to the system wd
        filepath = file.path(getwd(), directory)
        coltitle <- c("ID", "nobs")
        result <- matrix(,0,2)
        colnames(result) <- coltitle
        for(i in id){
                # attach the file with the path
                tmpfilepath = file.path(filepath,paste(formatC(i,width=3, format="d", flag = "0"), ".csv",sep = ""))
                mydata = read.csv(tmpfilepath)
                # find out how many rows in a file that has both salfate and nitrate
                tmp <- na.omit(mydata)
                ##total_means <- total_means + tmp_means
                result <- rbind(c(i,nrow(tmp)))
        }
        
        return(result)
} 