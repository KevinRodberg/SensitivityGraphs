#------------------------------------------------------------------------------------------------------------------------------------------------------------
#  Developed by: Kevin A. Rodberg, Science Supervisor 
#                 Resource Evaluation Section, Water Supply Bureau, SFWMD
#                 (561) 682-6702
#
#  September 2019
#
#  Script is provided multiple graphs comparing Model Sensitivty Analysis 
#  for the ECFTX within CFWI and the entire model domain.
#
#	SensitivityGraphs.R
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#   package management:
#     provide automated means for first time use of script to automatically 
#	  install any new packages required for this code, with library calls 
#	  wrapped in a for loop.
#-------------------------------------------------------------------------------
pkgChecker <- function(x){
  for( i in x ){
    if( ! require( i , character.only = TRUE ) ){
      install.packages( i , dependencies = TRUE )
      require( i , character.only = TRUE )
    }
  }
}
list.of.pkgs <-  c("readxl", "dplyr", "reshape2","ggplot2", "gridExtra","cowplot")

suppressWarnings(pkgChecker(list.of.pkgs))

#-------------------------------------------------------------------------------
# Identify path for input and output
#-------------------------------------------------------------------------------
outDir<-choose.dir(caption='Select output location for')
dir.create(file.path(outDir, "plots"), showWarnings = TRUE)

#-------------------------------------------------------------------------------
# Read Excel Data file and pre-process into Sens dataframe
#-------------------------------------------------------------------------------
# Parameter	Multiplier	Average MAE	Difference	Aquifer	Domain	Name
# SY_L3	    0.25	      2.46	      -0.03	      LFA	    CFWI	  LFA CFWI
# SY_L3	    0.2	        2.46	      -0.03	      LFA	    CFWI	  LFA CFWI
# SY_L3	    0.15	      2.46	      -0.03	      LFA	    CFWI	  LFA CFWI
# SY_L3	    0.05	      2.46	      -0.03	      LFA	    CFWI	  LFA CFWI
# SY_L3	    0.25	      2.62	      -0.02	      LFA	    ECFTX	  LFA ECFTX
# SY_L3	    0.2	        2.62	      -0.02	      LFA	    ECFTX	  LFA ECFTX

#-------------------------------------------------------------------------------
SensXl <- read_excel("R:/SensitivityGraphs/Sens_Graphics.xlsx", 
                   sheet='Sheet1', 
                   col_types=c("text","numeric","numeric","numeric",
                               "text","text","text"),                         
                   range = "a1:G4000")

Sens <-SensXl[SensXl$Parameter != 'Calibration', ]
Sens <- Sens[order(Sens$Parameter,Sens$Multiplier),]
Sens<- Sens[!is.na(Sens$Parameter),]
Sens <- mutate_if(Sens, is.character, as.factor)

Sens$Name <-factor(Sens$Name, levels=c("SAS CFWI","UFA CFWI","LFA CFWI",
                                       "SAS ECFTX","UFA ECFTX","LFA ECFTX"))

titles <- levels(Sens$Parameter)

#-------------------------------------------------------------------------------
# Create default Multiplier and Difference records
#-------------------------------------------------------------------------------
defaults <- SensXl[FALSE,]
defaults <-cbind(expand.grid(Parameter=titles,Name=levels(Sens$Name)),
                 Multiplier=1,Difference=0.0)
defaults[setdiff(names(Sens), names(defaults))] <- NA

#-------------------------------------------------------------------------------
# bind default records into previously read data
#-------------------------------------------------------------------------------
Sens<-rbind(Sens,defaults)
Sens <- Sens[order(Sens$Parameter,Sens$Multiplier),]
Sens$Multiplier <- as.factor(Sens$Multiplier)

#-------------------------------------------------------------------------------
# Create list of plots by 'Parameter' using titles from levels(Sens$Parameter)
#-------------------------------------------------------------------------------
plots <- lapply(titles, function(title, Sens){
  cat(paste('creating plot for:', title,'\n'))
  ggplot(data=Sens[Sens$Parameter==title,],
         aes(x=Multiplier, y= Difference, group=Name,
             color = Name, linetype=Name))+
    geom_path(size=1) +
    scale_color_manual(values= c("red","black","blue",
                                 "red","black","blue"))+
    scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash",
                                   "solid", "solid","solid")) +
    scale_x_discrete(expand=expand_scale(add=0.1)) +
    ylab("Deviation from \nCalibrated MAE") +
    theme_light() +
    theme(title = element_text(size=10),
          legend.text=element_text(size=5),
          legend.title=element_blank(),
          axis.text.x = element_text(size=10),
          axis.title.x = element_text(size=7),
          axis.text.y = element_text(size=10),
          axis.title.y = element_text(size=7)) +
    ggtitle(title)
}, Sens = Sens)

#-------------------------------------------------------------------------------
# Set up variables to layout multiple plots per page
#-------------------------------------------------------------------------------
ngrfs <- length(titles)
gPpg <- 6
pages<-data.frame(cbind(strt=seq(1,ngrfs,gPpg),
                        end=seq(gPpg,(ceiling(ngrfs/gPpg)*gPpg),gPpg)))
plots<- c(plots,rep(ggplot(),(ceiling(ngrfs/gPpg)*gPpg)-ngrfs))

#-------------------------------------------------------------------------------
# Blank plots are needed to fill in plot space when total number of
# graphs is not even multiple of plots per page
#-------------------------------------------------------------------------------
for (idx in seq(length(plots)+1,(ceiling(ngrfs/gPpg)*gPpg) )){
  plots[[idx]]<- ggplot()
}

#-------------------------------------------------------------------------------
# Plot multiple plots per page to .png files
#-------------------------------------------------------------------------------
for (pg in seq(1,length(pages[,1]))){
  cat(paste(pages[pg,]$strt,'-',pages[pg,]$end,'\n'))
  p<-cowplot::plot_grid(plotlist=
                          plots[pages[pg,]$strt:pages[pg,]$end],nrow=3,align="v")
  
  pngFile = paste(outDir, "/plots/", "Page",pg,"_grf",
                  pages[pg,]$strt,'-',pages[pg,]$end, 
                  ".png", sep = "")
  save_plot(pngFile,p,base_width=10, base_height=6.5)
  
}


