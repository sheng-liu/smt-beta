#### exportTrackll.R
#### Wu Lab, Johns Hopkins University
#### Author: Sun Jay Yoo
#### Date: July 11, 2017

## exportTrackll-methods
##
##
###############################################################################
##' @name exportTrackll
##' @aliases exportTrackll
##' @title exportTrackll
##' @rdname exportTrackll-methods
##' @docType methods
##'

##' @details
##' outputRowWise is only compatible with track lists with the fourth frame record column.
##' 
##' outputColWise is compatible with track lists both with or without the fourth frame record column.


##' @export .exportRowWise
##' @export .exportColWise
##' @export exportTrackll

###############################################################################

#### .exportRowWise ####

.exportRowWise = function(track.list){
    
    #Confirmation text of function call
    cat("Writing .csv row-wise output in current directory for", getTrackFileName(track.list), "...\n");
    
    #Empty data frame df to be written into the .csv
    df <- NULL;
    
    #Loop through every trajectory in input track.list
    for (i in 1:length(track.list)){
        
        #Create a data frame temp with trajectory, frame, and track coordinate data 
        #If the track list does not have a fourth frame record column, it will just output the start frame of each track instead
        if (length(track.list[[i]]) == 4){
            temp <- data.frame("trajectory" = i, "frame" = track.list[[i]][4], track.list[[i]][1:3]);
        } else {
            temp <- data.frame("trajectory" = i, "start.frame" = getStartFrame(track.list, i), track.list[[i]][1:3]);
        }
        
        #Append data frame df with data frame temp
        df <- rbind(df, temp);
    }
    
    #Write the data frame df into the .csv and display confirmation text
    file.name = paste(getTrackFileName(track.list), "Row.csv", sep = "")
    write.csv(df, file=file.name);
    cat(paste(file.name, "placed in current directory.\n", sep =""))
}

#### .exportColWise ####

#Install packages and dependencies
#library(plyr)

.exportColWise = function(track.list){
    
    #Confirmation text of function call
    cat("Writing .csv column-wise output in current directory for", getTrackFileName(track.list), "...\n");
    
    #Empty data frame df to be written into the .csv
    df <- NULL;
    
    #Loop through every trajectory in input track.list
    for (i in 1:length(track.list)){
        
        #Create temporary data frame to be filled with transposed lists from track.list
        temp <- NULL;
        for (j in 1:3){
            var <- data.frame(t(track.list[[i]][j]));
            temp <- rbind(temp, var);
        }
        
        #Append data frame df for .csv with temporary data frame
        df <- rbind.fill(df, temp);
    }

    #Write the data frame df into the .csv and display confirmation text
    file.name = paste(getTrackFileName(track.list), "Col.csv", sep = "")
    write.csv(df, file=file.name);
    cat(paste(file.name, "placed in current directory.\n", sep =""))
}

#### exportTrackll ####

exportTrackll = function(trackll, rowWise = T, colWise = T){
    export <- lapply(trackll,function(x){
        if (rowWise){
            .exportRowWise(track.list = x)
        }
        if (colWise){
            .exportColWise(track.list = x)
        }
    })
    
    
}
