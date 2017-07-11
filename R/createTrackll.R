#### createTrackll.R
#### Wu Lab, Johns Hopkins University
#### Author: Sun Jay Yoo
#### Date: July 11, 2017

## createTrackll-methods
##
##
###############################################################################
##' @name createTrackll
##' @aliases createTrackll
##' @title createTrackll
##' @rdname createTrackll-methods
##' @docType methods
##'
##' @description take in Diatrack (.txt or .mat), ImageJ (.csv), or SlimFast (.txt) input from a folder to output a list of track lists with the option to merge, mask, censor, record frames, and use multiple cores.

##' @usage 
##' createTrackll(folder, input = 0, interact = F, merge = F, ab.track = F, mask = F, cores = parallel::detectCores(logical=F), censorSingle = F, frameRecord = T)

##' @param folder Full path output file folder (if they are .txt, ensure that they are either all Diatrack or all SlimFast)
##' @param input Input file type (Diatrack .txt file = 1; Diatrack .mat session file = 2; ImageJ .csv file = 3; SlimFast .txt file = 4)
##' @param merge An logical indicate if the output list should be merged into one. Default merge = FALSE, output list is divided by file names.
##' @param ab.track Use absolute coordinates for tracks
##' @param mask A logical indicate if image mask should be applied to screen tracks. Default False. Note the mask file should have the same name as the Diatrack output txt file with a "_MASK.tif" ending. Users can use plotMask() and plotTrackOverlay() to see the mask and its effect on screening tracks.
##' @param cores Number of cores used for parallel computation. This can be the cores on a workstation, or on a cluster. Tip: each core will be assigned to read in a file when paralelled.
##' @param censorSingle Remove and censor trajectories that do not have a recorded next/previous frame (trajectories that appear for only one frame)
##' @param frameRecord add a fourth column to the track list after the xyz-coordinates for the frame that coordinate point was found (especially helpful when linking frames)

##' @details
##' NOTE: Diatrack .txt and ImageJ (.csv) input does not have the ability create a frame record and censor/uncensor, it will only extract whatever has been done by default to the .txt file.
##'
##'  The naming scheme for the list of track list is as follows:
##'  
##'  Track List: [full name of input file]
##'  Track: 
##' 
##' [Last five characters of the file name].[Start frame #].[Length].[Track #].[Index # (will differ from Track # when merging)]
##' 
##' (Note: The last five characters of the file name, excluding the extension, cannot contain “.”)

##' @examples
##' 

##' @export createTrackll

###############################################################################

### createTrackll ###

createTrackll=function(folder, input = 0, interact = F, merge = F, ab.track = F, mask = F, cores = parallel::detectCores(logical=F), censorSingle = F, frameRecord = T){
    
    if (interact){
        cat("Enter input file type and press ENTER: \n")
        cat("1. Diatrack .txt file \n")
        cat("2. Diatrack .mat session file: \n")
        cat("3. ImageJ .csv file \n")
        cat("4. SlimFast .txt file \n")

        input <- readline();
    }

    if (input > 4 || input < 1){
        cat("Restart script with correct input.")
    }

    if (input == 1){
        file.type = ".txt";
    } else if (input == 2){
        file.type = ".mat";
    } else if (input == 3){
        file.type = ".csv";
    } else if (input == 4){
        file.type = ".txt";
    }

    trackll = list()
    track.holder = c()
    
    # getting a file list of Diatrack files in a directory
    file.list = list.files(path = folder, pattern = file.type, full.names = T)
    file.name = list.files(path = folder, pattern = file.type, full.names = F)
    folder.name=basename(folder)
    
    # read in mask
    mask.list=list.files(path=folder,pattern="_MASK.tif",full.names=T)
    
    if (mask==T & length(mask.list)==0){
        cat("No image mask file ending '_MASK.tif' found.\n")
    
    }
    
    
    # read in tracks
    # list of list of data.frames,
    # first level list of file names and
    # second level list of data.frames
    
    max.cores = parallel::detectCores(logical = F)
    
    if (cores == 1){
        
        for (i in 1:length(file.list)){

            if (input == 1){
                track.list = .readDiatrack(file=file.list[i],ab.track=ab.track)
            } else if (input == 2){
                track.list = .readDiaSessions(file = file.list[i], ab.track = ab.track, censorSingle = censorSingle, frameRecord = frameRecord)
            } else if (input == 3){
                track.list = .readParticleTracker(file=file.list[i],ab.track=ab.track)
            } else if (input == 4){
                track.list = .readSlimFast(file = file.list[i], ab.track = ab.track, censorSingle = censorSingle, frameRecord = frameRecord)
            }
            
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
        if (input == 1){
            parallel::clusterExport(cl,varlist=c(".readDiatrack","ab.track"),envir=environment())
        } else if (input == 2){
            parallel::clusterExport(cl,varlist=c(".readDiaSessions","ab.track", "censorSingle", "frameRecord"),envir=environment())
        } else if (input == 3){
            parallel::clusterExport(cl,varlist=c(".readParticleTracker","ab.track"),envir=environment())
        } else if (input == 4){
            parallel::clusterExport(cl,varlist=c(".readSlimFast","ab.track", "censorSingle", "frameRecord"),envir=environment())
        }    
        
        # trackll=parallel::parLapply(cl,file.list,function(fname){
        trackll=parallel::parLapply(cl,file.list,function(fname){

            if (input == 1){
                track.list = .readDiatrack(file=fname,ab.track=ab.track)
            } else if (input == 2){
                track.list = .readDiaSessions(file = fname, ab.track = ab.track, censorSingle = censorSingle, frameRecord = frameRecord)
            } else if (input == 3){
                track.list = .readParticleTracker(file=fname,ab.track=ab.track)
            } else if (input == 4){
                track.list = .readSlimFast(file = fname, ab.track = ab.track, censorSingle = censorSingle, frameRecord = frameRecord)
            }

            # add indexPerTrackll to track name
            indexPerTrackll=1:length(track.list)
            names(track.list)=mapply(paste,names(track.list),indexPerTrackll,sep=".")
            return(track.list)
        })
        
        # stop cluster
        cat("\nStopping clusters...\n")
        parallel::stopCluster(cl)
        
        names(trackll)=file.name
        # names(track)=file.name
        
    }
    
    # cleaning tracks by image mask
    # filtration by image mask
    if (mask==T){
        trackll=maskTracks(trackll=trackll,maskl=mask.list)
    }
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
    
    if (merge==T){
    
        # trackll naming scheme
        # if merge==F, list takes the name of individual file name within folder
        # file.name > data.frame.name
        # if merge==T, list takes the folder name
        # folder.name > data.frame.name
        
        # concatenate track list into one list of data.frames
        for (i in 1:length(file.list)){
            track.holder=c(track.holder,trackll[[i]])
        }
        
        # rename indexPerTrackll of index
        # extrac index
        Index=strsplit(names(track.holder),split="[.]")  # split="\\."
        
        # remove the last old indexPerTrackll
        Index=lapply(Index,function(x){
            x=x[1:(length(x)-1)]
            x=paste(x,collapse=".")})
        
        # add indexPerTrackll to track name
        indexPerTrackll=1:length(track.holder)
        names(track.holder)=mapply(paste,Index,
                                    indexPerTrackll,sep=".")
        
        # make the result a list of list with length 1
        trackll=list()
        trackll[[1]]=track.holder
        names(trackll)[[1]]=folder.name
        
        # trackll=track.holder
        }
        
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
