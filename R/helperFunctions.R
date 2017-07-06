## helperFunctions
#
################################################################################






##------------------------------------------------------------------------------
## .timeStamp
# add time stamp and file name as a unique signature of the output file
##'@export .timeStamp
.timeStamp=function(filename){

    basename=basename(filename)
    name=unlist(strsplit(basename,split="[.]"))
    fileName=paste(name[1],"-",format(Sys.time(),"%Y%m%d.%H%M%S"),sep="")
    return(fileName)

}

##------------------------------------------------------------------------------
## .readDiatrack

# validity check for dt less than track length (-1)
.valid=function(dt,track){

    # get track length of all tracks
    tracklen=dim(track)[1]

    if (dt >(tracklen-1)){
        stop("\ntrack length:\t",dim(track)[1],
             "\ndt:\t\t",dt,
             "\nTime interval (dt) greater than track length-1\n")
    }
}


.valid=function(dt,track){

    # get track length of all tracks
    tracklen=dim(track)[1]

    if (dt >(tracklen-1)){
        stop("\ntrack length:\t",dim(track)[1],
             "\ndt:\t\t",dt,
             "\nTime interval (dt) greater than track length-1\n")
    }
}






##------------------------------------------------------------------------------
## tracks.msda2smt

##' @export tracks.msda2smt
tracks.msda2smt=function(file){

    tracks.file=readMat(file)
    # file.name=basename(file)
    tracks.mat=tracks.file$tracks

    trackl.smt=lapply(tracks.mat, function(x){
        x=data.frame(x)
        x=x[,-1] # remove time column
        x=x/0.107  # change µm to pixel
        colnames(x)=c("x","y")
        x$z=rep(1,times=dim(x)[1])
        return(x)
    })

    trackll.smt=list(trackl.smt)
    names(trackll.smt)=basename(file)
    return(trackll.smt)

}

##------------------------------------------------------------------------------
## same.scale

same.scale=function(mixmdl.lst){

    scale=list()
    length(scale)=length(mixmdl.lst)
    names(scale)=names(mixmdl.lst)

    for (i in 1:length(mixmdl.lst)){

        den=density(mixmdl.lst[[i]]$x)
        scale.x=c(min=min(den$x),max=max(den$x))
        scale.y=c(min=min(den$y),max=max(den$y))
        scale[[i]]=data.frame(scale.x,scale.y)
    }

    scale.df=do.call(rbind.data.frame,scale)
    scale.same=data.frame(apply(scale.df,2,function(x){
        c(min=min(x),max=max(x))}))

    return(scale.same)

}

##------------------------------------------------------------------------------
## seedIt
##' @export seedIt
seedIt=function(expr,seed){
    if (is.null(seed)){
        seed=sample(0:647,1)
        set.seed(seed)
    }else{set.seed(seed)}

    note <- paste("\nRandom number generation seed",seed,"\n")
    cat(note)
    structure(expr, seed=seed)
}


