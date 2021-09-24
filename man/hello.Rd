\name{Limpet Palettes}
\alias{Limpet Palettes}
\title{Limpet Palettes}
\usage{
lim_palette("pal1",n, c("discrete","continuous"))
}
\description{
Contains palettes based on limpet painting and can produce new continuous palettes following the design of the [wesanderson](https://github.com/karthik/wesanderson) package (see example).

'Limpet_pals' is a list that contains the various palettes.
You can use lim_palette() to define any palette that you want to use.

Use pal.bands() to show specific or all palettes.


}
\examples{
lim_palette("pal1",15,"continuous")

ggplot(mtcars,aes(x=factor(cyl),fill=factor(gear)))+
     geom_bar()+
     scale_fill_manual(values=lim_palette("pal3"))

pal.bands(pal1,pal2,pal3,pal4,pal5,pal6,pal7,pal8,pal9,labels=names(Limpet_pals[1:9]))


}
