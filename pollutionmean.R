pollutionmean <- function(directory, pollutant, id=1:332){
        # 'directory' is a character vector of length 1 indicating
        # the location of the CSV files
        
        # 'pollutant' is a character vector of length 1 indicating
        # the name of the pollutant for which we will calculate the mean;
        # either "sulfate' or 'nitrate'
        
        # 'id' is an integer vector indicating the monitor ID numbers
        # to be used
        
        # Return the mean of the pollutant across all monitors list 
        # in the 'id' vector(ignoring NA values)
        ## NOTE: Do not round the result!
        ## if the id beyond or below the scope defined above the return with NA
        if (id[1] < 1 | id[length(id)]>332 ) {
                return(NA)
        }
        # append the working directory to the system wd
        filepath = file.path(getwd(), directory)
        tmp_means <- 0
        total_means <-0
        for(i in id){
                # attach the file with the path
                tmpfilepath = file.path(filepath,paste(formatC(i,width=3, format="d", flag = "0"), ".csv",sep = ""))
                mydata = read.csv(tmpfilepath)
                # calculating the mean of the specified pollutant
                tmp_means <- mean(mydata[[pollutant]], na.rm = TRUE) 
                total_means <- total_means + tmp_means
        }
        total_means <- total_means / length(id)
        return(total_means)
}

