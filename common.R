#turn off messages and warnings and make it so output isn't prefixed by anything,
#default is to put "##" in front of all output for some reason
#also set tidy to true so code is wrapped properly 
knitr::opts_chunk$set(message=FALSE, warning=FALSE, comment = "", cache = F, fig.align = "center")
options(width = 200)

packagesUsed = c("GGally", "DT", "ggplot2", "stringr", 
                 "tidyverse", "stringr", 'bookdown', 
                 'knitr', 'rmarkdown', "finalfit",
                 "heatmaply", "cowplot",
                 "fastmatch", "rwantshue", "lubridate", 
                 "plotly", "ggnewscale", "ggtree", "ggtreeExtra", "ComplexHeatmap", 
                 "dendextend", "magrittr", "ggpmisc", 
                 "ggthemes", "ggridges", "HaplotypeRainbows", "ggdist")
# oldw <- getOption("warn")
# oldmessage <- getOption("message")
# options(warn = -1, message = -1) 
suppressMessages(lapply(packagesUsed, require, character.only = TRUE))
#loaded = lapply(packagesUsed, require, character.only = TRUE)
# options(warn = oldw, message = oldmessage)

myFormula= x~y
# commonly used functions  
`%!in%` <- Negate(`%in%`)
is.notna <-function(x){
  return(!is.na(x))
}
scheme <- iwanthue(seed = 42, force_init = TRUE) 
palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]


# ggplot themes 
transparentBackground = theme(
  panel.background = element_rect(fill='transparent'), #transparent panel bg
  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  panel.grid.major = element_blank(), #remove major gridlines
  panel.grid.minor = element_blank(), #remove minor gridlines
  legend.background = element_rect(color = NA, fill='transparent'), #transparent legend bg
  legend.box.background = element_rect(color = NA, fill='transparent') #transparent legend panel
)
sofonias_theme_noTransparentBackground = theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank() )+
  theme(axis.line.x = element_line(color="black", size = 0.3),axis.line.y =
          element_line(color="black", size = 0.3))+
  theme(text=element_text(size=12, family="Helvetica"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12)) +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

sofonias_theme = sofonias_theme_noTransparentBackground +
  transparentBackground
sofonias_theme_backgroundTransparent = sofonias_theme

sofonias_theme_xRotate_noBackgroundTransparent = theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank() )+
  theme(axis.line.x = element_line(color="black", size = 0.3),axis.line.y =
          element_line(color="black", size = 0.3))+
  theme(text=element_text(size=12, family="Helvetica"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12)) +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(size=12, angle = -90, vjust = 0.5, hjust = 0)) 

sofonias_theme_xRotate = sofonias_theme_xRotate_noBackgroundTransparent + 
  transparentBackground

sofonias_theme_xRotate_backgroundTransparent = sofonias_theme_xRotate

create_dt <- function(x){
  DT::datatable(x,
                extensions = 'Buttons',
                options = list(dom = 'Blfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               lengthMenu = list(c(10,25,50,-1),
                                                 c(10,25,50,"All"))))
}



create_tabsetOfHtmlWidgets <- function(htmlObjectsList) {
  zz <- textConnection("foo", "w")
  sink(zz)
  cat("::: {.panel-tabset}\n")
  iwalk(htmlObjectsList, ~ {
    cat('## ', .y, '\n\n')
    tempList = list()
    tempList[["item"]] = .x
    print(htmltools::tagList(tempList))
    cat('\n\n')
  })
  cat(":::\n")
  sink()
  close(zz)
  paste0(foo, collapse = "\n")
}

