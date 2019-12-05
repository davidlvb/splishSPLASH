
dfParsed <- readRDS("data/MedImm1/AA60/currentParsedReads.rds")
mat <- readRDS("data/MedImm1/AA60/matrix.rds")
Rcpp::sourceCpp('modules/findLocalMaxima.cpp')


readsWithinRange <- function(selected, segAwindowStart, segAwindowEnd, segBwindowStart, segBwindowEnd){
  return(
    subset(selected, (  ((selected$start1 >= segAwindowStart) & (selected$start1 <= segAwindowEnd) |
                                     (selected$end1 >= segAwindowStart) & (selected$end1 <= segAwindowEnd) |
                                     (selected$start1 <= segAwindowStart) & (selected$end1 >= segAwindowEnd)) 
                                  &
                                    ((selected$start2 >= segBwindowStart) & (selected$start2 <= segBwindowEnd) |
                                       (selected$end2 >= segBwindowStart) & (selected$end2 <= segBwindowEnd) |
                                       (selected$start2 <= segBwindowStart) & (selected$end2 >= segBwindowEnd))        ))
  )
}

dfReadsWithinRange <- function(dfParsed, centers){
  centers <- add_column(centers, readsBounded = 0)
  for (i in 1:nrow(centers)){
    centers$readsBounded[i] <- nrow(readsWithinRange(dfParsed,
                                                 centers$minX[i], centers$maxX[i],
                                                 centers$minY[i], centers$maxY[i]))
  }
  return(arrange(centers, desc(readsBounded)))
}

log20 <- function(input){
  out <- log2(input)
  out[!is.finite(out)] <- 0
  return(out)
}

# For cimg, how to apply a colourpallete
# im <- (1+sin(.1*Xc(imfill(100,100))))/2
# hsv(h=.2,v=im)  %>% col2rgb  %>% t  %>% as.vector  %>% as.cimg(dim=c(dim(im)[1:3],3))  %>% plot


# Plot image with correct y axes
plot(mirror(as.cimg((mat)), "y"), ylim=c(1, height(mat)))
plot(mirror(as.cimg(log20(mat)), "y"), ylim=c(1, height(mat)))

# Plot image with correct y axes
blobMask <- (grow(as.pixset(as.cimg(findLocalMaxima_cpp(mat, 10, 30, 1.1))), 30))
plot(mirror(blobMask, "y"), ylim=c(1, height(blobMask)))

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

blobs[1,]


# blobs %>% 
#   mutate(readsBounded = purrr::pmap_dbl(list(start1, end1, start2, end2), readsWithinRange(dfParsed)))



points(centers$mx, centers$my, col="green", pch=5, cex=1.5, lwd=2)
points(centers$mx, centers$my, col="green", pch="o", cex=1, lwd=1)
