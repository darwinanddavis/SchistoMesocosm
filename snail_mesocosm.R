### schisto mesocosm ###
### NB: Date and Snail cols contain unnatural values ###

#12-6-18
#added stripchart for meso2 data

#11-6-18
# plotting function uses <<- operator

#7-6-18 
# created plotting function

# to do
# make phyto and peri vs NP  into grouped bargraph
# correlation coefficients between body mass (tissue) and parasite loading, periphyton consumed, diameter      
# turn main() function into PDF markdown output

# install dependencies
packages <- c("rgdal","dplyr","zoo","RColorBrewer","viridis","plyr","digitize","jpeg","devtools","imager","dplyr","ggplot2", "svDialogs")   
install.packages(packages,dependencies = T)
lapply(packages,library,character.only=T)

# get data
# /Users/malishev/Documents/Emory/research/mesocosm
wd <- dlgInput("Set working directory", "Your working dir (without quotation marks")$res # set working dir      
setwd(paste0(wd,"/")); list.files() # setwd and list files
f <- "meso1.csv"
f2 <- "meso2.csv"

# load data
meso1 <- read.table(f,header=T,sep=",", row.names=NULL,stringsAsFactors=FALSE, strip.white=TRUE) # read data
tail(meso1)
meso2 <- read.table(f2,header=T,sep=",",row.names=NULL,stringsAsFactors=FALSE, strip.white=TRUE) # read data
tail(meso2)
colnames(meso2)[2] <- "NP" # fix col names
meso1[is.na(meso1)] <- 0 ; meso2[is.na(meso2)] <- 0 # remove NAs

# set plotting graphics
plot_it <- function(manuscript,bg,cp,alpha,family){ # plotting function (plot for MS or not, set bg color, set color palette from RColorBrewer, set alpha value for transperancy) 
  graphics.off()
  if(manuscript==0){
    if(bg=="black"){
      colvec<-magma(200,1)
      par(bg = colvec[1],col.axis="white",col.lab="white",col.main="white",fg="white",bty="n",las=1,mar=c(5,6,4,2),family=family) #mono
      border=adjustcolor("purple",alpha=0.5)
    }else{
      colvec<-bpy.colors(200)
      par(bg = colvec[1],col.axis="white",col.lab="white",col.main="white",fg="white",bty="n",las=1,mar=c(5,6,4,2),family=family) 
      border=adjustcolor("blue",alpha=0.5)
    }
  }else{
    graphics.off()
    par(bty="n",las=1,family=family) 
  }
  # color palettes
  # ifelse(manuscript==1,colvec<-adjustcolor(brewer.pal(9,cp)[9], alpha = alpha),colvec <- adjustcolor(brewer.pal(9,cp)[5], alpha = alpha)) # fine tune plotting colors for plotting bg
   # colfunc <<- colorRampPalette(brewer.pal(9,cp),alpha=alpha)
  colfunc <<- adjustcolor(brewer.pal(9,cp),alpha=alpha) # USES <<- OPERATOR
}
plot_it(0,"blue","Blues",1,"HersheySans") # set col function params

# diameter
buffer <- 0.25
den <- density(meso1$Diameter)
plot(den,
     col=adjustcolor("lightblue",alpha=0.5),
     ylim=c(0,max(den$y)+(max(den$y)*buffer)),
     xlab="Diameter (mm)",
     ylab="Density",
     main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks"))
polygon(den, col=adjustcolor("lightblue",alpha=0.5),border="lightblue") # fill AUC 
abline(v=mean(meso1$Diameter),col="pink",lty=3,ylim=c(0,max(den$y)+(max(den$y)*buffer))) # get mean

# diameter over time (weeks)
buffer <- 0.25 # percentage buffer added to axes range 
boxplot(Diameter~Week, data=meso1,
        ylim=c(0,max(meso1$Diameter+(meso1$Diameter*buffer))),
        col = "light blue",
        notch = T,xlab="Week",ylab="Diameter (mm)",
        main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks")
        )
abline(h=mean(meso1$Diameter),col="pink",lty=3)

# diameter per tank
buffer <- 0.25
boxplot(Diameter~Tank, data=meso1,
        ylim=c(0,max(meso1$Diameter+(meso1$Diameter*buffer))),
        col = "light blue",
        notch = T,xlab="Tank",ylab="Diameter (mm)",
        main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks")
)
abline(h=mean(meso1$Diameter),col="pink",lty=3)
with(meso1,t.test(Diameter,Tank)) # t.test

# diameter v cercariae
# point size by cercariae number
par(mfrow=c(1,2))
with(meso1,plot(Diameter,Cercariae,pch=20,
                col=adjustcolor("light blue",alpha=0.5),
                cex=(meso1$Cercariae+1)/1000,
                ylab="Number of cercariae released over 90 mins",xlab="Diameter (mm)",
                ))
abline(v=mean(meso1$Diameter),lty=3,col="pink")# mean diameter
par(family="mono")
legend("topright",legend=paste0(unique(meso1$Sampling_Effort)," samples"),
       title ="Sampling effort",
       border="white",pch=20, col=brewer.pal(meso1$Sampling_Effort,"Blues")[1:3],
       pt.cex=3,
       bty="n")

# point size by sampling effort
with(meso1,plot(Diameter,Cercariae,pch=20,
                col=adjustcolor(brewer.pal(meso1$Sampling_Effort,"Blues")[1:3],alpha=0.3),
                cex=(meso1$Sampling_Effort)/1.5,
                ylab="Number of cercariae released over 90 mins",xlab="Diameter (mm)",
))
abline(v=mean(meso1$Diameter),lty=3,col="pink")# mean diameter
# legend("right",legend=paste0(unique(meso1$Sampling_Effort)," samples"),
#        title ="Sampling effort",
#        border="white",pch=20,col=brewer.pal(meso1$Sampling_Effort,"Blues")[1:3],
#        pt.cex=unique(meso1$Sampling_Effort),
#        bty="n",
#        # xjust=1,
#        adj=-2,
#        # y.intersp=0.5,
#        text.font=NULL)



meso2$Schisto <- as.integer(as.factor(meso2$Schisto))-1# convert Y/N in Schisto col to 1/0
# convert size to integers
meso2$Size <- gsub("Intermediate","2Intermediate",meso2$Size)  
meso2$Size <- gsub("Small","1Small",meso2$Size) 
meso2$Size <- gsub("Large","3Large",meso2$Size) 
meso2$Size <- as.integer(as.factor(meso2$Size))

#size vs egg mass
with(meso2,plot(Eggs~Size))

# N/P concentration v egg mass
with(meso2,stripchart(Eggs~NP,
                      method="jitter", jitter=0.1,
                      pch=20,
                      cex=meso2$Size,
                      col=adjustcolor("lightblue",alpha=0.3),
                      vertical=T,
                      group.names=c("High","Low"),
                      xlab="",
                      ylab="",
                      main="")
     )
abline(h=mean(meso2$Eggs),col="pink",lty=3)
title(main=paste0("Number of egg masses for high and low N/P \n levels over ",max(meso1$Week)," weeks"),
      xlab="N/P level")
title(ylab="Number of egg masses",line=3.5)

# phyto = flourescence units
# peri = flourescence per 2 weeks / 3.5 inch^2 tile (gross productivity biomass rate)

# N/P concentration v phyto
with(meso2,stripchart(Phyto_F~NP,
                      method="jitter", jitter=0.1,
                      pch=20,
                      cex=1,
                      col=adjustcolor("light blue",alpha=0.3),
                      vertical=T,
                      group.names=c("High","Low"),
                      xlab="",
                      ylab="",
                      main="")
     )
abline(h=mean(meso2$Eggs),col="pink",lty=3)
title(main=paste0("Phytoplankton concentration for high and low N/P \n levels over ",max(meso1$Week)," weeks"),
      xlab="N/P level")
title(,ylab="Phytoplankton concentration",line=3.5)

# N/P concentration v peri
with(meso2,stripchart(Peri_F~NP,
                      method="jitter", jitter=0.1,
                      pch=20,
                      cex=1,
                      col=adjustcolor("light blue",alpha=0.3),
                      vertical=T,
                      group.names=c("High","Low"),
                      xlab="N/P concentration",ylab="Periphyton concentration",
                      main=paste0("Phytoplankton concentration for high and low N/P \n levels over ",max(meso1$Week)," weeks")
))
abline(h=mean(meso2$Eggs),col="pink",lty=3)

# make phyto and peri vs NP  into grouped bargraph
buffer <- 0.3
xlim <- max(den$x);xlim
ylim <- max(den$y);ylim
den <- density(meso2$Phyto_F)
plot(den,
     col=adjustcolor("light blue",alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)),
     ylim=c(0,ylim+(ylim*buffer)),
     xlab="",
     ylab="",
     main=""
) 
polygon(den, col=adjustcolor("light blue",alpha=0.5),border="light blue") # fill AUC 
abline(v=mean(meso2$Phyto_F),col="light blue",lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
par(new=T) # add periphyton concentration
den2 <- density(meso2$Peri_F)
plot(den2,
     col=adjustcolor("orange",alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)), # uses xy lims from phyto
     ylim=c(0,ylim+(ylim*buffer)),
     xlab="",
     ylab="",
     main=""
     )
polygon(den2, col=adjustcolor("orange",alpha=0.5),border="orange") # fill AUC 
abline(v=mean(meso2$Peri_F),col="orange",lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
title(main=paste0("Resource concentration over ",max(meso1$Week)," weeks"),
      xlab="Resource concentration")
title(ylab="Density",line=3.5)
      

# N/P concentration v presence of schisto

# egg mass over time w/ presence of schisto. overlay phyto and pero conc as density polygon 

# egg mass v phyto

# egg mass v peri

# diameter dists for different NP ranges
# https://www.statmethods.net/graphs/density.html

# diameter vs phyto

# diameter vs peri

# diameter vs egg mass (with schisto)

# joyplot of egg mass over time for each size class


#after sourcing above functions and codes
# print results output
main<-function(){
  # set plot windows
  par(mfrow = c(1,2))
  print("Part1")
  # run plot function
  plot_it(1,"black","Greens",0.5,"HersheySans")
  print("Part2")
  p<-round(lahat,3)
  print("phat")
  print(p)
  print("Part3")
  # run another function
  tanksim(1,3000,1000,phat,mhat,lahat,T)
  print("Part4")
  # run another function
  cost()
  #set plot windows
  par(mfrow = c(1,1))
}


