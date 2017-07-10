#### readDiaSessions.R
#### Wu Lab, Johns Hopkins University
#### Author: Sun Jay Yoo
#### Date: July 10, 2017

## readDiaSessions-methods
##
##
###############################################################################
##' @name readDiaSessions
##' @aliases readDiaSessions
##' @title readDiaSessions
##' @rdname readDiaSessions-methods
##' @docType methods
##'
##' @description take in a Diatrack .mat session file as input, along with several other user-configurable parameters and output options, to return a track list of all the trajectories found in the session file
                                         
##' @usage 
##' readDiaSessions(file, interact = TRUE, ab.track = FALSE, censorSingle = TRUE, frameRecord = TRUE)
##'
##' removeFrameRecord(track.list)
##' 
##' outputRowWise(track.list)
##' 
##' outputColWise(track.list)

##' @param file Full path to Diatrack .mat session file
##' @param interact Open menu to interactively choose file
##' @param ab.track Use absolute coordinates for tracks
##' @param censorSingle Remove and censor trajectories that do not have a recorded next/previous frame (trajectories that appear for only one frame)
##' @param frameRecord add a fourth column to the track list after the xyz-coordinates for the frame that coordinate point was found (especially helpful when linking frames)

##' @details
##' The naming scheme for each track is as follows:
##' 
##' [Last five characters of the file name].[Start frame #].[Length].[Track #]
##' 
##' (Note: The last five characters of the file name, excluding the extension, cannot contain “.”)
##' 
##' removeFrameRecord is a helper script aims to make track lists with a fourth frame record column backwards compatible 
##' with other smt functions that rely on track lists with only three columns for the xyz-coordinates. 
##' The fourth column is simply removed from the given track list.
##' 
##' outputRowWise is only compatible with track lists with the fourth frame record column.
##' 
##' outputColWise is compatible with track lists both with or without the fourth frame record column.

##' @examples
##' #Basic function call of readDiaSessions
##' trackll <- readDiaSessions()
##' 
##' #Function call of readDiaSessions without a frame record and output to .csv files
##' trackll2 <- readDiaSessions(frameRecord = F, rowWise = T, colWise = T)
##' 
##' #Option to output .csv files after processing the track lists
##' outputRowWise(trackll)
##' outputColWise(trackll)
##' 
##' #To find your current working directory
##' getwd()
##' 
##' #Remove default fourth frame record column
##' trackll.removed <- removeFrameRecord(trackll)
##' 

##' @export .readDiaSessions
##' @export removeFrameRecord
##' @export outputColWise
##' @export outputRowWise
##' @export readDiaSessions

##' @importFrom R.matlab readMat

###############################################################################

#### Note ####

#This script takes Diatrack .mat files as input, and returns a list of data frames (a track list) of all the particle trajectories.
#The aim is to optimize and un-censor this process, instead of having to use MATLAB to extract a large .txt file which is then fed into R.

#Additional features:
#Adding frame records, removing frame records, outputing column-wise and row-wise to .csv files, linking skipped frames

#### Testing ####

#A .mat session file with 10117 frames was used to test both scripts.

#Using the MATLAB script, a 272.6MB .txt file was first created and was then fed into the readDiatrack() script to output track lists. 
#Automating this process using "matlabr" resulted in 4488 censored tracks (should be 4487 tracks since the script does not censor first frame) in 3:48 mins.

#Using readDiaSessions, the intermediate .txt file was no longer needed to be created and the session file directly results in track lists.
#This script resulted in 4487 censored tracks in 2:00 mins and 34689 uncensored tracks in 2:01 mins. 

#### readDiaSessions ####

#Install packages and dependencies
#install.packages("R.matlab")
#library(R.matlab)

.readDiaSessions = function(file, interact = F, ab.track = F, censorSingle = F, frameRecord = T){
    
    #Interactively open window
    if (interact == TRUE) {
        file=file.choose();
    }
    
    #Start timer
    #if (timer == TRUE) {
    #    start.time = Sys.time()
    #}
    
    #Collect file name information
    file.name = basename(file);
    file.subname = substr(file.name, start=nchar(file.name)-8, stop=nchar(file.name)-4);
    
    #Display starter text
    cat("\nReading Diatrack session file: ",file.name,"...\n");
    
    #Pre-process data
    #Successor and predecessor rows of first frame switched for consistency
    #(Unsure why Diatrack reverses the ordering of these two rows for the first frame)
    data <- readMat(file)$tracks;
    temp <- data[1][[1]][[1]][[7]]; 
    data[1][[1]][[1]][[7]] <- data[1][[1]][[1]][[6]];
    data[1][[1]][[1]][[6]] <- temp;
    
    #Data structure of data for future reference:
    #data[FRAME][[1]][[1]][[ROW]][[COL]] 
    
    #Instantiate indexing variables, the track list, and the start frame list
    startIndex = 0;
    startFrame = 1;
    trajectoryIndex = 1;
    track.list = list();
    frame.list = list();
    length.list = list();
    
    #Loop for each trajectory track to be saved into track list
    repeat{
        
        #Loop until the next particle without a predecessor is found
        repeat{
            
            #Basic iteration through indexes and then through frames
            if (startIndex == 0 || startIndex < ncol(data[[startFrame]][[1]][[1]])){
                startIndex = startIndex + 1;
            } else {
                
                
                startFrame = startFrame + 1;
                startIndex = 1;
            }
            
            #Check at each iteration
            if (startFrame > length(data)){ #Break at end frame
                break;
            } else if (length(data[startFrame][[1]][[1]][[1]]) == 0){ #Iterate to next frame at empty frames
                next;
            } else if (data[startFrame][[1]][[1]][[6]][[startIndex]] == 0) { #Break if particle is found
                break;
            }
            #Do nothing and iterate to next indexed particle if no particle is found
        }
        
        #Break track loop at end frame
        if (startFrame > length(data)){
            break;
        }
        
        #Instantiate initial frame and index coordinates into looping frame and index coordinates
        frame = startFrame;
        index = startIndex;
        
        #Create temporary track to insert into track list
        track <- data.frame(x = numeric(), y = numeric(), z = integer());
        
        #Loop through every instance the particle exists and add its data to track
        #Break once it no longer has successors
        repeat{
            RefinedCooX = round(data[frame][[1]][[1]][[2]][[index]], 2);
            RefinedCooY = round(data[frame][[1]][[1]][[1]][[index]], 2);
            RefinedCooZ = round(data[frame][[1]][[1]][[3]][[index]], digits = 1);
            if (!frameRecord){
                track <- rbind(track, data.frame(x = RefinedCooX, y = RefinedCooY, z = RefinedCooZ));
            } else {
                track <- rbind(track, data.frame(x = RefinedCooX, y = RefinedCooY, z = RefinedCooZ, frame = frame));
            }
            if (data[frame][[1]][[1]][[7]][[index]] != 0) {
                index = data[frame][[1]][[1]][[7]][[index]];
                frame = frame + 1;
            } else
                break;
        }
        
        #Check for censoring tracks that appear for only frame
        if (!censorSingle || nrow(track) != 1){
            
            #Add start frame to frame list
            frame.list[[length(frame.list) + 1]] <- startFrame;
            
            #Add track length to length list
            length.list[[length(length.list) + 1]] <- nrow(track);
            
            #Calcualte absolute track coordinates if desired
            if (ab.track){
                track <- abTrack(track);
            }
            
            #Append temporary track for particle into track list and iterate to the next trajectory
            track.list[[trajectoryIndex]] <- track;
            trajectoryIndex = trajectoryIndex + 1;
        }
    }
    #Name track list:
    #[Last five characters of the file name without extension (cannot contain ".")].[Start frame #].[Length].[Track #]
    names(track.list) = paste(file.subname, frame.list, length.list, c(1:length(track.list)), sep=".");
    
    #File read and processedconfirmation text
    cat("Session file read and processed.\n")
    
    #Display stop timer
    #if (timer == TRUE) {
    #    end.time = Sys.time();
    #    time.taken = end.time - start.time;
    #   cat("Duration: ");
    #    cat(time.taken);
    #    cat(" mins\n");
    #}
    
    #Return track list
    return(track.list);
} 

#### removeFrameRecord ####

removeFrameRecord = function(track.list){
    for (i in 1:length(track.list)){
        track.list[[i]] <- track.list[[i]][-c(4)];
    }
    return (track.list);
}

#### outputRowWise ####

#MUST USE TRACK LIST WITH FRAME RECORD

outputRowWise = function(track.list){
    
    #Confirmation text of function call
    cat("Writing .csv row-wise output in current directory...\n");
    
    #Empty data frame df to be written into the .csv
    df <- NULL;
    
    #Loop through every trajectory in input track.list
    for (i in 1:length(track.list)){
        
        #Create a data frame temp with trajectory, frame, and track coordinate data 
        temp <- data.frame(Trajectory = i, Frame = track.list[[i]][4], track.list[[i]][1:3]);
        
        #Append data frame df with data frame temp
        df <- rbind(df, temp);
    }
    
    #Write the data frame df into the .csv and display confirmation text
    write.csv(df, file="outputRow.csv");
    cat("outputRow.csv placed in current directory.\n\n");
}

#### outputColWise ####

#Install packages and dependencies
#library(plyr)

outputColWise = function(track.list){
    
    #Confirmation text of function call
    cat("Writing .csv column-wise output in home directory...\n");
    
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
    write.csv(df, file="outputCol.csv");
    cat("outputCol.csv placed in home directory.\n\n");
}

### readDiaSessions ###

readDiaSessions=function(folder, merge = F, ab.track = F, mask = F, cores = 1, censorSingle = T, frameRecord = T){
    
    trackll = list()
    track.holder = c()
    
    # getting a file list of Diatrack files in a directory
    file.list = list.files(path = folder, pattern = ".mat", full.names = T)
    file.name = list.files(path = folder, pattern = ".mat", full.names = F)
    folder.name=basename(folder)
    
    # read in mask
    #mask.list=list.files(path=folder,pattern="_MASK.tif",full.names=T)
    
    #if (mask==T & length(mask.list)==0){
    #    cat("No image mask file ending '_MASK.tif' found.\n")
        
    #}
    
    
    # read in tracks
    # list of list of data.frames,
    # first level list of file names and
    # second level list of data.frames
    
    max.cores = parallel::detectCores(logical = F)
    
    if (cores == 1){
        
        for (i in 1:length(file.list)){
            
            track.list = .readDiaSessions(file = file.list[i], ab.track = ab.track, censorSingle = censorSingle, frameRecord = frameRecord)
            
            # add indexPerTrackll to track name
            indexPerTrackll = 1:length(track.list)
            names(track.list) = mapply(paste, names(track.list), indexPerTrackll,sep = ".")
            
            trackll[[i]] = track.list
            names(trackll)[i] = file.name[i]
        }
        
    } else {
        
        # parallel this block of code
        # assign reading in using .readDiatrack to each CPUs
        
        # detect number of cores
        # FUTURE: if more than one, automatic using multicore
        
        if (cores>max.cores)
            stop("Number of cores specified is greater than recomended maximum: ", max.cores)
        
        cat("Initiated parallel execution on", cores, "cores\n")
        # use outfile="" to display result on screen
        cl <- parallel::makeCluster(spec = cores,type = "PSOCK", outfile = "")
        # register cluster
        parallel::setDefaultCluster(cl)
        
        # pass environment variables to workers
        parallel::clusterExport(cl,varlist=c(".readDiaSessions","ab.track", "censorSingle", "frameRecord"),envir=environment())
        
        # trackll=parallel::parLapply(cl,file.list,function(fname){
        trackll=parallel::parLapply(cl,file.list,function(fname){
            track=.readDiaSessions(file=fname,ab.track=ab.track, censorSingle = censorSingle, frameRecord = frameRecord)
            # add indexPerTrackll to track name
            indexPerTrackll=1:length(track)
            names(track)=mapply(paste,names(track),indexPerTrackll,sep=".")
            return(track)
        })
        
        # stop cluster
        cat("Stopping clusters...\n")
        parallel::stopCluster(cl)
        
        names(trackll)=file.name
        # names(track)=file.name
        
    }
    
    # cleaning tracks by image mask
    #if (mask==T){
    #    trackll=maskTracks(trackll=trackll,maskl=mask.list)
    #}
    
    # merge masked tracks
    # merge has to be done after mask
    
    
    # if (merge==T){
    #     for (i in 1:length(file.list)){
    #         trackll[[i]]=track[[i]]
    #         names(trackll)[i]=file.name[i]
    #     }
    # }
    
    
    # trackll naming scheme
    # if merge==F, list takes the name of individual file name within folder
    # file.name > data.frame.name
    # if merge==T, list takes the folder name
    # folder.name > data.frame.name
    
    # filtration by image mask
    #if (mask==T){
    #    trackll=maskTracks(trackll=trackll,maskl=mask.list)
    #}
    
    # merge masked tracks
    # merge has to be done after mask
    
    #if (merge==T){
        
        # trackll naming scheme
        # if merge==F, list takes the name of individual file name within folder
        # file.name > data.frame.name
        # if merge==T, list takes the folder name
        # folder.name > data.frame.name
        
        # concatenate track list into one list of data.frames
        #for (i in 1:length(file.list)){
        #    track.holder=c(track.holder,trackll[[i]])
        #}
        
        # rename indexPerTrackll of index
        # extrac index
        #Index=strsplit(names(track.holder),split="[.]")  # split="\\."
        
        # remove the last old indexPerTrackll
        #Index=lapply(Index,function(x){
            #x=x[1:(length(x)-1)]
            #x=paste(x,collapse=".")})
        
        # add indexPerTrackll to track name
        #indexPerTrackll=1:length(track.holder)
        #names(track.holder)=mapply(paste,Index,
                                   #indexPerTrackll,sep=".")
        
        # make the result a list of list with length 1
        #trackll=list()
        #trackll[[1]]=track.holder
        #names(trackll)[[1]]=folder.name
        
        # trackll=track.holder
    #}
    
    #     }else{
    #
    #         # list of list of data.frames,
    #         # first level list of folder names and
    #         # second level list of data.frames
    #
    #         for (i in 1:length(file.list)){
    #
    #             track=.readDiatrack(file=file.list[i],ab.track=ab.track)
    #             # concatenate tracks into one list of data.frames
    #             track.holder=c(track.holder,track)
    #
    #         }
    #
    #         # add indexPerTrackll to track name
    #         indexPerTrackll=1:length(track.holder)
    #
    #         names(track.holder)=mapply(paste,names(track.holder),
    #                                    indexPerTrackll,sep=".")
    #
    #         # make the result a list of list with length 1
    #         trackll[[1]]=track.holder
    #         names(trackll)[[1]]=folder.name
    #
    
    #
    #
    #         if (mask==T){
    #             trackll=maskTracks(trackll,mask.list)
    #         }
    #
    #     }
    
    return(trackll)
}