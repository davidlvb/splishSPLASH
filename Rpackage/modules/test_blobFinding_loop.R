

findBlobs <- function(dfParsed, blobMask){
  # Find blobs using label function and calculate position, bounding box, and num pixels
  blobs <- dplyr::group_by(subset(as.data.frame(label(blobMask)), value > 0), value) %>% 
    dplyr::summarise(mx=mean(x),my=mean(y), 
                     minX=min(x), maxX=max(x), 
                     minY=min(y), maxY=max(y),
                     npixel=n())
  
  numBlobsFound <- nrow(blobs)
  
  # Count number of reads that fall within the bounding box of each row of blobs
  # (note, uses a for loop, and assumes blobs has named columns as defined above)
  blobs <- dfReadsWithinRange(dfParsed, blobs)
  
  blobs <- select(blobs, -value)
  blobs$normRPKC <- 1000*blobs$readsBounded/((blobs$maxX-blobs$minX+1)*(blobs$maxY-blobs$minY+1))
  blobs <- arrange(blobs, desc(readsBounded))
  return(blobs)
}




refGenome <- readRDS("data/Medimm1/AA60/AA60_refGenome.rds")
dfParsed <- as.tibble(fread(file = "data/Medimm1/AA60/AA60_filteredReadTable.txt"))
segments <- refGenome$refGenomeDF$segment
segmentLengths <- refGenome$refGenomeDF$length
dfParsed$chr1 <- factor(dfParsed$chr1, levels=segments)
dfParsed$chr2 <- factor(dfParsed$chr2, levels=segments)

# Determine how many matrices we need (i.e. interaction pairs) 
numChr <- length(segments)
numChr <- 8
# For not including 'self'
numPairs <- (numChr^2)/2 - (numChr/2)  # If we included self interactions, it would be:  numMatrices <- (numChr^2)/2 + (numChr/2)

# Set up a matrix to list the segment pairs we will test
segPairs <- data.frame(seg1=rep("", numPairs), seg2=rep("", numPairs), stringsAsFactors=FALSE)
rowCount <- 1
for (i in 1:(numChr-1)){  # remove "-1" to include self interactions, and...
  for (j in (i+1):numChr){ # ...remove "+1" to include self interactions
    segPairs$seg1[rowCount] <- segments[i]
    segPairs$seg2[rowCount] <- segments[j]
    rowCount <- rowCount + 1
  }
}


# setProgress <- shiny::setProgress
## withProgress(min=0, max=numPairs, message = "Finding local maxima",{

tic()
  for (i in 1:numPairs){
    
    cat(paste(i, " of ", numPairs, "—————————————", sep=""))
    tic()
    rowsToSelect <- which(
      ((dfParsed$chr1 %in% segPairs$seg1[i]) & (dfParsed$chr2 %in% segPairs$seg2[i])) |
        ((dfParsed$chr1 %in% segPairs$seg2[i]) & (dfParsed$chr2 %in% segPairs$seg1[i]))
    )
    # swapChrReadTable ensure segmentA is in chr1
    currentDFparsed <- swapChrReadTable(dfParsed[rowsToSelect,], segPairs$seg1[i], segPairs$seg2[i])
    currentMatrixDim <- c( segmentLengths[which(segments == segPairs$seg1[i])] , segmentLengths[which(segments == segPairs$seg2[i])] )
    
    # generate interaction matrix
    mat <- generateInteractionMatrix_cpp(currentDFparsed, currentMatrixDim)
    blobMask <- (grow(as.pixset(as.cimg(findLocalMaxima_cpp(mat, 20, 30, 1.1))), 30))
    
    # Run blob-finding algorithm
    blobs <- findBlobs(dfParsed, blobMask)
    
    # Add column to indicate which segment
    blobs <- add_column(blobs, seg1=rep(segPairs$seg1[i], nrow(blobs)), .before=1)
    blobs <- add_column(blobs, seg2=rep(segPairs$seg2[i], nrow(blobs)), .after=1)

    if(exists("blobsOut")){
      blobsOut <- bind_rows(blobsOut, blobs)
    } else {
      blobsOut <- blobs
    }
    toc()
  }
blobsOut$normRPKC <- 1000*blobsOut$readsBounded/((blobsOut$maxX-blobsOut$minX+1)*(blobsOut$maxY-blobsOut$minY+1))
blobsOut <- arrange(blobsOut, desc(readsBounded))
cat("—————————————Done")
toc()
## })



