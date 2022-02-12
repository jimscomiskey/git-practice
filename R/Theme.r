# 1/12/2013 changes made to element from theme for newer version
#


fontsize=30

MIDNTheme <- function(base_size = fontsize, base_family="nps") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(family=base_family, size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1, vjust=1, angle = 45, margin=margin(t=5)),
      axis.text.y =       element_text(family=base_family, size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1, vjust= -.5),
      axis.ticks =        element_line(colour = "black"),
      axis.title.x =      element_text(family=base_family, size = base_size, face ="bold", vjust= 1, debug=F),
      axis.title.y =      element_text(family=base_family, size = base_size, angle = 90, hjust= .5,vjust=0.5, face="bold", debug=F, margin = margin(r= 10)),
      axis.ticks.length = unit(0.15, "cm"),
      # axis.ticks.margin = unit(0.1, "cm"),
      
      legend.background = element_rect(fill="gray99", size=.25, linetype="solid"),
      legend.key =        element_rect(fill = "grey95", colour = "white"),
      legend.key.size =   unit(1.2, "lines"),
      legend.text =       element_text(family=base_family, size = base_size * 0.7, colour="dark green", face = "italic"),
      legend.title =      element_text(family=base_family, size = base_size * 0.8, face = "bold", hjust = 0, colour="black"),
      legend.position =   "right",
      legend.key.height = NULL, 
      legend.key.width =  NULL, 
      legend.text.align = NULL, 
      legend.title.align= NULL, 
      legend.direction =  "vertical", 
      legend.box =        "vertical",
      #          
      panel.background =  element_rect(colour = "black"),   
      panel.border =      element_blank(),
      panel.grid.major =  element_line(colour = "grey90"),   #   element_line(colour = "white")
      panel.grid.minor =  element_blank(), # element_line(colour = "grey95", size = 0.25)
      panel.spacing    =      unit(0.25, "cm"),
      
      strip.background =  element_rect(fill = "grey80", colour = NA, size=1),
      strip.text.x =      element_text(family=base_family, size = base_size * 0.8),
      strip.text.y =      element_text(family=base_family,size = base_size * 0.8, angle = -90, face="bold",  hjust=.5, vjust=0.75),
      
      plot.background =   element_blank(),
      plot.title =        element_text(family=base_family,size = base_size * 1.2, lineheight=8, face="bold", vjust=3, hjust=.5, margin = margin(t= 1,b = 10)),
      plot.margin =       unit(c(1, 1, 1, 1), "cm"),
      
      complete = TRUE
    )}


MIDNTheme_horiz <- function(base_size = fontsize, base_family="nps") {
	 theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
					axis.line =         element_blank(),
					axis.text.x =       element_text(family=base_family,size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", hjust = 1, vjust=1, angle = 0), 
					axis.text.y =       element_text(family=base_family,size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1),
					axis.ticks =        element_line(colour = "grey50"),
					axis.title.x =      element_text(family=base_family,size = base_size,  hjust=.7,vjust=-1,face="bold"),
					axis.title.y =      element_text(family=base_family,size = base_size, angle = 90, hjust=.5,vjust=.3,face="bold"),
					axis.ticks.length = unit(0.15, "cm"),
					axis.ticks.margin = unit(0.1, "cm"),

					legend.background = element_rect(fill="gray99", size=.25, linetype="solid"),
					legend.key =        element_rect(fill = "grey95", colour = "white"),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       element_text(family=base_family,size = base_size * 0.7, colour="dark green", face = "italic"),
					legend.title =      element_text(family=base_family,size = base_size * 0.8, face = "bold", hjust = 0, colour="black"),
					legend.position =   "right",
          legend.key.height = NULL, 
          legend.key.width =  NULL, 
          legend.text.align = NULL, 
          legend.title.align= NULL, 
          legend.direction =  "vertical", 
          #legend.box =        "vertical",
          
					panel.background =  element_rect(colour = "black"),   
					panel.border =      element_blank(),
					panel.grid.major =  element_line(colour = "grey90"),   #   element_line(colour = "white")
					panel.grid.minor =  element_blank (), # element_line(colour = "grey95", size = 0.25)
					panel.spacing =      unit(0.25, "lines"),

					strip.background =  element_rect(fill = "grey80", colour = NA),
					strip.text.x =      element_text(family=base_family,size = base_size * 0.8),
					strip.text.y =      element_text(family=base_family,size = base_size * 0.8, angle = -90, face="bold",  hjust=.5, vjust=0.75),

					plot.background =   element_blank(),
					plot.title =        element_text(family=base_family,size = base_size * 1.2, lineheight=.8, face="bold"),
					plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines") ,
					
				 complete = TRUE
			)}

MIDNTheme_blankplot <- function(base_size = fontsize, base_family="nps") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      
      complete = TRUE
      )}

