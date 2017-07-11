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

##' @description take in a list of track lists (trackll) and export it into row-wise and/or column-wise .csv files in the working directory

##' @usage 
##' exportTrackll(trackll, rowWise = T, colWise = T, cores = parallel::detectCores(logical=F))
##' 
##' .exportRowWise(track.list)
##' 
##' .exportColWise(track.list)
##' 

##' @param trackll a list of track lists
##' @param rowWise option to use Image-J style row-wise output in .csv files
##' @param colWise optio to use Diatrack style col-wise output in .csv files
##' @param cores Number of cores used for parallel computation. This can be the cores on a workstation, or on a cluster. Tip: each core will be assigned to read in a file when paralelled.
##' @param track.list a single track list

##' @details
##' For .exportRowWise, if the track list does not have a fourth frame record column, it will just output the start frame of each track instead
##' 
##' It is not recommended that exportTrackll be run on merged list of track lists (trackll).
##' 
##' Ensure that the input trackll is a list of track lists and not just a trackl track list

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

exportTrackll = function(trackll, rowWise = T, colWise = T, cores = parallel::detectCores(logical=F)){
    
    # detect number of cores
    max.cores=parallel::detectCores(logical=F)
    
    if (cores==1){
        export = lapply(trackll,function(x){
            if (rowWise){
                .exportRowWise(track.list = x)
            }
            if (colWise){
                .exportColWise(track.list = x)
            }
        })
    } else {
        # parallel excecute above block of code
        if (cores>max.cores)
            stop("Number of cores specified is greater than maxium: ",
                 max.cores)
        
        cat("Initiated parallel execution on", cores, "cores\n")
        
        # use outfile="" to display result on screen
        cl <- parallel::makeCluster(spec=cores,type="PSOCK",outfile="")
        # register cluster
        parallel::setDefaultCluster(cl)
        
        # pass environment variables to workers
        parallel::clusterExport(cl,
                                varlist=c(".exportRowWise",".exportColWise"),
                                envir=environment())
        
        export = parallel::parLapply(cl,trackll,function(x){
            if (rowWise){
                .exportRowWise(track.list = x)
            }
            if (colWise){
                .exportColWise(track.list = x)
            }
        })
        
        # stop cluster
        cat("Stopping clusters...\n")
        parallel::stopCluster(cl)
    }
    
}
