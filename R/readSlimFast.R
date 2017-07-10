#### readSlimFast.R
#### Wu Lab, Johns Hopkins University
#### Author: Sun Jay Yoo
#### Date: July 10, 2017

## readSlimFast-methods
##
##
###############################################################################
##' @name readSlimFast
##' @aliases readSlimFast
##' @title readSlimFast
##' @rdname readSlimFast-methods
##' @docType methods
##'
##' @description take in a SlimFast .txt session file as input, along with several other user-configurable parameters and output options, to return a track list of all the trajectories

##' @usage 
##' readSlimFast(file, interact = FALSE, censorSingle = TRUE)

##' @param file Full path to SlimFast .txt session file
##' @param interact Open menu to interactively choose file
##' @param ab.track Use absolute coordinates for tracks
##' @param censorSingle Remove and censor trajectories that appear for only one frame
##' @param frameRecord add a fourth column to the track list after the xyz-coordinates for the frame that coordinate point was found (especially helpful when linking frames)

##' @details
##' The naming scheme for each track is as follows:
##' 
##' [Last five characters of the file name].[Start frame #].[Length].[Track #]
##' 
##' (Note: The last five characters of the file name, excluding the extension, cannot contain “.”)

##' @examples
##' 

##' @export .readSlimFast

###############################################################################


#### .readSlimFast ####

.readSlimFast = function(file, interact = F,  ab.track = F, censorSingle = F, frameRecord = T){
    
    #Interactively open window
    if (interact == TRUE) {
        file=file.choose();
    }
    
    #Collect file name information
    file.name = basename(file);
    file.subname = substr(file.name, start=nchar(file.name)-8, stop=nchar(file.name)-4);
    
    #Display starter text
    cat("\nReading SlimFast file: ",file.name,"...\n");
    
    #Read first four columns of input SLIMFAST file
    data <- as.data.frame(subset(read.table(file), select=c(1:4)));
    
    #Name columns and add z column of 1s in the appropriate location
    colnames(data) <- c("x","y","frame", "track");
    data <- cbind(data, data.frame("z" = 1));
    data <- data[,c("x","y","z","frame", "track")]
    
    #Instantiate track, start frame, and length lists
    track.list = list();
    frame.list = list();
    length.list = list();
    
    #Instantiate counter indexing variable and extract last trajectory
    counter = 1
    last.trajectory = data[nrow(data), ][[5]];
    
    #Loop through every given trajectory number
    for (i in 1:last.trajectory){
        
        #Create track data frame
        track <- data.frame();
        
        #Loop through every line of the input file with a globally updating counter index
        repeat{
            
            #If trajectory number is equal to the track number, add line to track and update counter
            if (i == as.integer(data[counter, ][[5]]) && counter <= nrow(data)){
                track <- rbind(track, data[counter, ]);
                counter = counter + 1;
            } else { #Break out of loop if they are unequal and the track ends
                break;
            }
        }
        
        #Check for censoring tracks that appear for only frame
        if (!censorSingle || nrow(track) != 1){
            
            #Remove track number column from track
            track <- track[-c(5)];
            
            #Option to add/remove frame record
            if (!frameRecord){
                track <- track[-c(4)];
            }
            
            #Calcualte absolute track coordinates if desired
            if (ab.track){
                track <- abTrack(track);
            }
            
            #Rename row names of track to appropriate index values
            rownames(track) <- 1:nrow(track);
            
            #Add start frame of track to frame list
            frame.list[[length(frame.list) + 1]] <- track[[4]][[1]];
            
            #Add track length to length list
            length.list[[length(length.list) + 1]] <- nrow(track);
            
            #Append temporary track data frame into track list
            track.list[[i]] <- track;
        }
    }
    
    #Name track list:
    #[Last five characters of the file name without extension (cannot contain ".")].[Start frame #].[Length].[Track #]
    names(track.list) = paste(file.subname, frame.list, length.list, c(1:length(track.list)), sep=".");
    
    #Return track list
    return (track.list);
}
