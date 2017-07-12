#### exportTrackll.R
#### Wu Lab, Johns Hopkins University
#### Author: Sun Jay Yoo
#### Date: July 12, 2017

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
##' exportTrackll(trackll, rowWise = T, colWise = T, cores = 1)
##' 
##' .exportRowWise(track.list)
##' 
##' .exportColWise(track.list)
##' 

##' @param trackll a list of track lists
##' @param rowWise option to use Image-J style row-wise output in .csv files
##' @param colWise option to use Diatrack style col-wise output in .csv files
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

    frame.list <- list()
    
    #Loop through every trajectory in input track.list
    for (i in 1:length(track.list)){
        
        start.frame = getStartFrame(track.list[i])
        
        frame.list <- c(frame.list, start.frame, 0, 0)
        
        temp <- track.list[[i]][1:3]
        
        if (i != 1){
            df <- cbind.fill(df, temp, fill = 0) 
        } else {
            df <- temp
        }
    }
    
    colnames(df) <- frame.list
    
    header = "format (columnwise): Frame1 row n+1: (y(tn) x(tn) z(tn)), row n+1: (y(t(n+1)) x(t(n+1)) z(t(n+1))), row n+2: (y(t(n+2)) x(t(n+2) z(t(n+2)) y(t(n+3)).... where Frame1 is the frame number where the target is seen for the first time, and the columns define trajectories. Beware! the number of tracks is limited by the width of the widest text file on your machine. Rowwise export preferred"

    #Write the data frame df into the .csv and display confirmation text
    file.name = paste(getTrackFileName(track.list), "Col.csv", sep = "")
    write(header, file = file.name, append = T)
    write.table(df, file = file.name, row.names = FALSE, sep = "\t", append = T);
    
    cat(paste(file.name, "placed in current directory.\n", sep =""))
}

#### exportTrackll ####

exportTrackll = function(trackll, rowWise = T, colWise = T, cores = 1){
    
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
