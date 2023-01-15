
########################################################################
### Developed by:   Petar Milin                                      ###
### Last modified:  07-02-2022                                       ###
###                                                                  ###
### Note: Also part of the package that accompanies the paper        ###
###       "From their point of view: the article category            ###
###        as a hierarchically structured referent tracking system"  ###
###       To appear in Linguistics                                   ###
########################################################################

### Converts dense matrix into a sparse ###
make_sparse <- function(d) {
    c = sort(unique(unlist(strsplit(as.character(d[,1]), split='_'))))
    o = sort(unique(unlist(strsplit(as.character(d[,2]), split='_'))))
    mx = matrix(0, nrow(d), (length(c)+length(o)),
        dimnames=list(NULL, c(c, o)))
    mc = lapply(strsplit(as.character(d[,1]), '_'), FUN=match, colnames(mx))
    mo = lapply(strsplit(as.character(d[,2]), '_'), FUN=match, colnames(mx))
    cell = do.call(rbind,
        lapply(seq_along(mo), function(x){cbind(x, c(mc[[x]], mo[[x]]))}))
    mx[cell] = 1
    #print(paste('NoCues = ', length(c), '; NoOutcomes = ', length(o), sep=''))
    return(mx)
}

### Widrow-Hoff ###
update_w <- function(invec, teachval, wold, lambda) {
    wnext = wold + (lambda * invec %*% (teachval - t(invec) %*% wold))
    return(wnext)
}

# -----

run_whoff_epoch <- function(dfr, noTarget, cNames, oNames, lRate=0.01) {
    # Initialize learning
    trainPats = as.matrix(dfr)
    trainPats = trainPats[,c(cNames,oNames)]       # order cues and outcomes
    nTarg = noTarget                               # no targets (teachers)
    nPats = dim(trainPats)[1]                      # no training patterns
    nCues = dim(trainPats)[2] - nTarg              # no of inputs
    W = matrix(0, nrow=nCues, ncol=nTarg,          # weight matrix
               dimnames = list(cNames, oNames)) 
    Lr = lRate                                     # learning rate (alpha)
    wList = list()
    # Training (updating)
    for (trialIdx in 1:nPats) {
        avec = matrix(trainPats[trialIdx,1:nCues], ncol=1)
        tval = matrix(trainPats[trialIdx,nCues+1:nTarg], nrow=1)
        W = update_w(avec, tval, W, Lr)
        wList[[trialIdx]] = W
    }
    # Reformat into a long data frame
    nameGrid = expand.grid(cNames, oNames)
    d = data.frame(
        Trial = rep(seq(nPats), each=dim(nameGrid)[1]),
        Cues = rep(nameGrid[,1], nPats),
        Outcomes = rep(nameGrid[,2], nPats),
        Weights = unlist(wList)
    )
    return(list(d = d, W = W))
}


