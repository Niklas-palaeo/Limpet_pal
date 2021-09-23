#All of this is copied code from the pals package

pal.bands <- function(..., n=100, labels=NULL, main=NULL, gap=0.1, sort="none", show.names=TRUE){

  #if(n < 3) warning("Using n=3")
  if(!is.element(sort, c("none","hue","luminance")))
    stop("'sort' must be one of 'none','hue','luminance'")

  # Each argument in '...' is a palette function or palette vector.
  # if a function, use n colors
  # if a vector, use all colors in the palette

  pals <- list(...)
  isfun <- unlist(lapply(pals, is.function))
  npal <- length(pals)

  if(!is.null(labels)) {
    if(length(labels) != npal)
      stop("Length of labels needs to match number of palettes.")
  } else {
    # Get the palette function name, or blank
    # Once a function is passed as an argument, the name of the function is gone,
    # so we have to use 'match.call' to get the names
    mc <- match.call()
    labels <- unlist(lapply(mc, deparse))
    labels <- labels[-1] # first item is 'pal.bands'
    labels <- labels[1:npal] # other arguments n, labels
    labels <- ifelse(isfun, labels, "")
  }

  # Now convert the colormap functions to palette vectors
  for(i in 1:npal) {
    if(isfun[i]) pals[[i]] <- pals[[i]](n)
  }
  # Count the number of boxes for each palette
  nc <- unlist(lapply(pals, length))

  # AFTER functions are converted to vectors, we can sort if needed
  if(sort=="hue"){
    for(i in 1:npal){
      hsvcol <- methods::as(colorspace::hex2RGB(pals[[i]]), "HSV")@coords
      pals[[i]] <- pals[[i]][order(hsvcol[,1],hsvcol[,2])]
    }
  }
  if(sort=="luminance"){
    for(i in 1:npal){
      cols <- methods::as(colorspace::hex2RGB(pals[[i]]), "LUV")@coords
      pals[[i]] <- pals[[i]][order(cols[,1],cols[,2])]
    }
  }


  maxn <- max(nc)
  ylim <- c(0, npal)
  # mgp: The margin line (in mex units) for the axis title, axis labels and axis line.
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, maxn), ylim = ylim,
       type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "")

  # draw a band for each palette
  for (i in 1:npal) {
    # i goes bottom to top, npal+1-i goes top to bottom
    nj <- nc[npal+1 - i]
    shadi <- pals[[npal+1 - i]]
    brks <- seq(from=0, to=maxn, length=nj+1) # horiz break points between colors
    # the vertical height of each bar is 1-gap, with 'gap' white fraction at the top
    rect(xleft = brks[1:nj], ybottom = i-1,
         xright = brks[2:(nj+1)], ytop = i-gap, col = shadi, border = NA)

    # If inidividual colors in a palette have names, add them
    nms <- names(shadi)
    if(show.names & !is.null(nms)) {
      textcol <- ifelse(col2rgb(colorspace::desaturate(shadi))['red',] < 128,
                        "white", "black")
      text(brks[1:nj] + 0.5, i-.6, nms, srt=90, cex=.75, col=textcol)
    }
  }

  # Palette name along left side
  text(rep(-0.2, npal), (1:npal) - 0.6,
       labels = rev(labels),
       cex=0.6, xpd = TRUE, adj = 1)


  if(!is.null(main)) title(main)

  invisible()
}
