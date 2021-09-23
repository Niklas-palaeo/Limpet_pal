
#List of palettes

Limpet_pals <- list(
  pal1 = c("#3F4244","#816F57","#42423A"),
  pal2 = c("#9B9877","#8C7652"),
  pal3 = c("#545C5F","#847367","#9AA099","#A39D83","#B4A597"),
  pal4 = c("#8D9A8E","#646E60","#86928E","#D2CABE"),
  pal5 = c("#3D4144","#C0B8A4"),
  pal6 = c("#9B8B6F","#7E7B68","#AAA17A","#757A6B"),
  pal7 = c("#98A06D","#9CA183"),
  pal8 = c("#746048","#C3B898"),
  pal9 = c("#D6B47D","#5D5346","#C3BB9C","#BA9D7B","#CDC397")
)



# Make own palette based on the wesanderson package

lim_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- Limpet_pals[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

