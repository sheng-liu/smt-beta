#### readDiaSessions.R
#### Wu Lab, Johns Hopkins University
#### Author: Sun Jay Yoo
#### Date: June 23, 2017



##' @export readDiaSessions
##' @export removeFrameRecord
##' @export outputColWise
##' @export outputRowWise
##' @export linkSkippedFrames

##' @importFrom R.matlab readMat


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
#All function inputs, except the timer and interact, was set as FALSE

#### readDiaSessions ####

#Install packages and dependencies
#install.packages("R.matlab")
library(R.matlab)

#PARAMETERS: 
#file = filepath input if desired, interact = open menu to choose file
#censorSingle = skip tracks that only appear for one frame
#rowWise = output .csv file in home directory of tracks in row-wise (ImageJ style) organization using outputRowWise function call
#colWise = output .csv file in home directory of tracks in column-wise (ImageJ stye) using output
#timer = time duration of function
#frameRecord = add a column for frame number for each coordinate point

readDiaSessions = function(file, interact = TRUE, censorSingle = TRUE, frameRecord = TRUE, rowWise = FALSE, colWise = FALSE, timer = FALSE){
    
    #Interactively open window
    if (interact == TRUE) {
        file=file.choose();
    }
    
    #Start timer
    if (timer == TRUE) {
        start.time = Sys.time()
    }
    
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
            
            #Append temporary track for particle into track list and iterate to the next trajectory
            track.list[[trajectoryIndex]] <- track;
            trajectoryIndex = trajectoryIndex + 1;
        }
    }
    #Name track list:
    #[Last five characters of the file name without extension (cannot contain ".")].[Start frame #].[Length].[Track #]
    names(track.list) = paste(file.subname, frame.list, length.list, c(1:length(track.list)), sep=".");
    
    #File read and processedconfirmation text
    cat("Session file read and processed.\n\n")
    
    #Return ImageJ style row-wise output with trajectory and frame numbers in .csv file
    if (rowWise) { 
        outputRowWise(track.list);
    } 
    
    #Return column-wise output in .csv file
    if (colWise) {
        outputColWise(track.list);
    } 
    
    #Display stop timer
    if (timer == TRUE) {
        end.time = Sys.time();
        time.taken = end.time - start.time;
        cat("Duration: ");
        cat(time.taken);
        cat(" mins\n");
    }
    
    #Return track list
    return(track.list);
} 

#### removeFrameRecord ####

#PARAMETERS:
#track.list = track list output with frame record

removeFrameRecord = function(track.list){
    for (i in 1:length(track.list)){
        track.list[[i]] <- track.list[[i]][-c(4)];
    }
    return (track.list);
}

#### getStartFrame ####

#PARAMETERS: 
#track.list = named track list output
#Note: Last five characters of the original file name without extension (cannot contain ".")
#index = index in track list

getStartFrame = function(track.list, index){
    return(as.numeric(substr(names(track.list[index]), 
                             gregexpr(pattern = '\\.', names(track.list[index]))[[1]][1]+1, 
                             gregexpr(pattern = '\\.', names(track.list[index]))[[1]][2]-1)));
}

#### outputColWise ####

#Install packages and dependencies
library(plyr)

#PARAMETERS: 
#track.list = track list output from readDiatrack or readDiaSessions

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
    cat("output.csv placed in home directory.\n\n");
}

#### outputRowWise ####

#MUST USE TRACK LIST WITH FRAME RECORD

#PARAMETERS: 
#track.list = track list output from readDiatrack or readDiaSessions

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
    cat("output.csv placed in current directory.\n\n");
}

#### linkSkippedFrames ####

#PARAMETERS: 
#track.list = track list output from readDiatrack or readDiaSessions
#tolerance = tolerance level in pixels
#maxSkip = maximum number of frame skips

linkSkippedFrames = function(track.list, tolerance, maxSkip){
    
    #Confirmation text of function call
    cat("Linking trajectories with a tolerance of",  tolerance, "and a maximum frame skip of", maxSkip,  "...\n");
    
    #Instantiate empty linked track list
    track.list.linked = list();
    
    #Instantiate frame/length/linknum list for track naming
    frame.list = list();
    length.list = list();
    linknum.list = list();
    
    #Instantiate starting frame of the last trajectory, max frame after skips, and index
    lastTrajFrame = getStartFrame(track.list, length(track.list));
    maxFrame = maxSkip + 1;
    trajectoryIndex = 1;
    
    #Extract file sub-name from given track list
    file.subname = substr(names(track.list[1]), 1, gregexpr(pattern = '\\.', names(track.list[1]))[[1]][1] - 1);
    
    #Loop through each new linked trajectory
    repeat{
        
        #Extract first trajectory in track list into data frame temp for linking trajectories
        temp <- track.list[[1]];
        
        #Record the start amd last frame of the starting trajectory from track name
        startFrame = getStartFrame(track.list, 1);
        lastFrame = getStartFrame(track.list, 1) + nrow(temp) - 1;
        
        #Delete the extracted first trajectory from track list and shift list to replace
        #The first trajectory in the track list is the next trajectory after the data frame temp
        track.list[[1]] <- NULL
        
        #Record the last X and Y coordinate values of data frame temp
        lastX = temp[[1]][[nrow(temp)]];
        lastY = temp[[2]][[nrow(temp)]];
        
        #Set link counter to 0;
        linkCounter = 0;
        
        #Instantiate index i and loop through trajectories in track list to look for frame skips 
        i = 1;
        repeat{
            
            #If the starting frame of the trajectory is beyond the maximum skips possible or the last frame, break
            nextFrame =  getStartFrame(track.list, i);
            if (i > length(track.list) || nextFrame > lastFrame + maxFrame){
                break;
            }
            
            #Record the first X and Y coordinate values of trajectory
            nextX = track.list[[i]][[1]][[1]];
            nextY = track.list[[i]][[2]][[1]];
            
            #Check if the first XY values of trajectory and the last XY values in temp are within the set tolerance
            if (abs(lastX-nextX) <= tolerance && abs(lastY-nextY) <= tolerance){
                
                #Update lastFrame
                lastFrame = getStartFrame(track.list, i) + nrow(track.list[[i]]) - 1;
                
                #Append trajectory to temp
                temp <- rbind(temp, track.list[[i]]);
                
                #Delete trajectory from track list and shift
                track.list[[i]] <- NULL
                
                #Recalculate last X and Y coordinate values of appended data frame temp
                lastX = temp[[1]][[nrow(temp)]];
                lastY = temp[[2]][[nrow(temp)]];
                
                #Increment link counter
                linkCounter = linkCounter + 1;
                
            } else {
                
                #Increment index to next trajectory if nothing is found
                i = i + 1;
            }
        }
        
        #Add start frame to frame list
        frame.list[[length(frame.list) + 1]] <- startFrame;
        
        #Add track length to length list
        length.list[[length(length.list) + 1]] <- nrow(temp);
        
        linknum.list[[length(linknum.list) + 1]] <- linkCounter;
        
        #Append data frame temp of the fully linked trajectory into track.list.linked and increment index
        track.list.linked[[trajectoryIndex]] <- temp;
        trajectoryIndex = trajectoryIndex + 1;
        
        #Break if the track list of decreasing length is empty 
        if (length(track.list) == 0){
            break;
        }
    }
    
    #Name track list:
    #[File name from input].[Start frame #].[Length].[Track #].[# of links]
    names(track.list.linked) = paste(file.subname, frame.list, length.list, c(1:length(track.list.linked)), linknum.list, sep=".");
    
    #Return linked track list and confirmation text
    cat(Reduce("+", linknum.list), "links found.\n\n");
    
    return (track.list.linked);
}

#### Extra and useless functions ####

findMaxLinks = function(track.list, maxTolerance = 10, maxMaxSkip = 500){
    minLength = length(track.list)
    minTolerance = maxTolerance
    minSkip = maxMaxSkip
    cat("Searching...");
    for (i in 1:maxTolerance){
        for (j in 1:maxMaxSkip){
            length = length(linkSkippedFrames(track.list, tolerance = i, maxSkip = j, print = F));
            if (length < minLength){
                minLength = length
                minTolerance = i
                minSkip = j
            }
            
        }
    }
    cat("For the maximum number of links, with minimum tolerance and then minimum skips:")
    cat("\nTolerance =",  tolerance)
    cat("\nSkips =",  minSkip)
    cat("\nLength =",  minLength)
}


