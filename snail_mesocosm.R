### schisto mesocosm ###
### NB: Date and Snail cols contain unnatural values ###

#21-6-18
# separated un/infected data

#15-6-18
# updated cerc production per tank plot to only select tanks with cerc produced and to include upper limit across all tanks
 
#13-6-18
# phyto = flourescence units
# peri = flourescence per 2 weeks / 3.5 inch^2 tile (gross productivity biomass rate)
#added global cex params for plotting different cex sizes, e.g. cercariae, sampling effort

#12-6-18
#added stripchart for meso2 data

#11-6-18
# plotting function uses <<- operator

#7-6-18 
# created plotting function

# to do
# TO DO subset only by snails infected
# TO DO biomass =  The published weight-length regression that I mentioned last week is:
    # [Soft tissue dry mass in mg] = 0.0096 * Diameter[in mm]^3
# Try population Pyramid plot for infected v uninfected /Users/malishev/Documents/Melbourne Uni/Programs/R code
# correlation coefficients between body mass (tissue) and cerc produced, periphyton consumed, diameter      
# turn main() function into PDF markdown output

##### install dependencies
packages <- c("rgdal","dplyr","zoo","RColorBrewer","viridis","plyr","digitize","jpeg","devtools","imager","dplyr","ggplot2", "svDialogs")   
install.packages(packages,dependencies = T)
lapply(packages,library,character.only=T)

##### get data
# /Users/malishev/Documents/Emory/research/mesocosm
wd <- dlgInput("Set working directory", "Your working dir (without quotation marks")$res # set working dir      
setwd(paste0(wd,"/")); list.files() # setwd and list files
f <- "meso1.csv"
f2 <- "meso2.csv"

#### load data
meso1 <- read.table(f,header=T,sep=",", row.names=NULL,stringsAsFactors=FALSE, strip.white=TRUE) # read data
tail(meso1)
meso2 <- read.table(f2,header=T,sep=",",row.names=NULL,stringsAsFactors=FALSE, strip.white=TRUE) # read data
tail(meso2)
colnames(meso2)[2] <- "NP" # fix col names
meso1[is.na(meso1)] <- 0 ; meso2[is.na(meso2)] <- 0 # remove NAs

# cleaning Snail and Date cols
unique(meso1$Snail)
unique(meso1$Date)
sapply(meso1, function(x) sum(nchar(x))) # check number of characters in each col

# set cex sizes
cex_cer <- (meso1$Cercariae+1)/1000
cex_sam <- meso1$Sampling_Effort/1.5
cex_diam <- meso1$Diameter/3

#### set plotting graphics
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
print("1/0, set colour, set colour palette 'display.brewer.all()',set alpha for col,set font")
plot_it(0,"black","Blues",1,"HersheySans") # set col function params

##############################################################################

## Snail diameter (mm) distribution
# _______________________________________________ compare un/infected snails 

buffer <- 0.25
col <- "lightblue"
den <- density(meso1$Diameter)
xlim <- max(den$x);xlim; ylim <- max(den$y);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)),
     ylim=c(0,ylim+(ylim*buffer)),
     xlab="Diameter (mm)",
     ylab="Density",
     main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks"))
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(meso1$Diameter),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean

### Snail size over time (weeks)
# Shell diameter (mm) over time (weeks)
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 
buffer <- 0.25 # percentage buffer added to axes range 
boxplot(Diameter~Week, data=meso1,
        # xlim=c(0,max(meso1$Week)),
        ylim=c(0,max(meso1$Diameter+(meso1$Diameter*buffer))),
        col = "light blue",
        notch = T,xlab="Week",ylab="Diameter (mm)",
        main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks"),
        xaxs = "i", yaxs = "i"
        )
abline(h=mean(meso1$Diameter),col="pink",lty=3)
par(new=T)
points(x=c(1,3,5,7),y=rep(30,4),pch=6,col="red")# add inoculation points

### Snail size per tank 
# Shell diameter (mm) per tank
# _______________________________________________ compare un/infected snails 
buffer <- 0.25
boxplot(Diameter~Tank, data=meso1,
        ylim=c(0,max(meso1$Diameter+(meso1$Diameter*buffer))),
        col = "light blue",
        notch = T,xlab="Tank",ylab="Diameter (mm)",
        main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks")
)
abline(h=mean(meso1$Diameter),col="pink",lty=3)
with(meso1,t.test(Diameter,Tank)) # t.test

### Snail size and number of cercariae produced
# Point size by cercariae number
col <- "lightblue"
with(meso1,plot(Diameter,Cercariae,pch=20,
                col=adjustcolor("light blue",alpha=0.5),
                cex=cex_cer,
                ylab="Number of cercariae released over 90 mins",xlab="Diameter (mm)",
                ))
abline(v=mean(meso1$Diameter),lty=3,col=col)# mean diameter
# legend(c(50,5500),legend=paste0(unique(meso1$Sampling_Effort)," samples"),
#        title ="Sampling effort",
#        border="white",pch=20,col=brewer.pal(meso1$Sampling_Effort,"Blues")[1:3],
#        pt.cex=unique(meso1$Sampling_Effort),
#        bty="n",
#        # xjust=1,
#        adj=-2,
#        # y.intersp=0.5,
#        text.font=NULL)

### Snail size per tank
# Shell diameter (mm) 
# _______________________________________________ compare un/infected snails 
# @@@ joyplot
tank <- 3 # select tank #. max 48
snail <- subset(meso1,subset=Tank==tank) # get tank level indiviudals
buffer <- 0.25
diam_total <- 1 # set ylim either to max for tank or max across all tanks (16.8) 
col <- "lightblue"
den <- density(snail$Diameter) # get diameter density
xlim <- max(den$x);xlim
ylim <- max(den$y);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)),ylim=c(0,ylim+(ylim*buffer)),
     xlab="",ylab="",main=""
)
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(snail$Diameter),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
title(main=paste0("Shell diameter (mm) distribution for tank #",tank),
      xlab="Shell diameter (mm)")
title(ylab="Density",line=3.5)
  
### Individual cercariae production over time
# Cercariae shed over 90 mins per week
### ~1000 eggs inoculated at 0,2,4,6 weeks
tank <- 21 # max 48 
cer_total <- 0 # set ylim either to max for tank or max across all tanks (6100) 

snail <- subset(meso1,subset=Cercariae>0);snail # get only cercariae
snail <- subset(snail,subset=Tank==tank);snail # get tank level indiviudals
buffer <- 0.25 # axis buffer
xlim <- max(meso1$Week) # uses total num of weeks
ifelse(cer_total==1,ylim <- round_any(max(meso1$Cercariae),100),ylim <- max(snail$Cercariae))
col <- "lightblue" 
if(length(snail$Cercariae)>0){
  with(snail,plot(Cercariae~Week,
                  col=adjustcolor(col,alpha=0.5),
                  type="h",
                  lwd=5,
                  xlim=c(0,xlim),ylim=c(0,ylim+(ylim*buffer)),
                  xlab="",ylab="",main=""
  ))
  abline(h=mean(snail$Cercariae),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
  title(main=paste0("Cercariae production for tank ",tank," over ",max(meso1$Week)," weeks"),
        xlab="Week")
  title(ylab="Number of cercariae shed in 90 mins",line=3.5)
  par(new=T)
  points(x=c(0,2,4,6),y=rep(30,4),pch="~",cex=1.5,col="red")# add inoculation points
}else{print("No cercariae in this tank")}

############################################################################################################

# Mesocosm 2 data sheet
meso2$Schisto <- as.integer(as.factor(meso2$Schisto))-1# convert Y/N in Schisto col to 1/0
# convert size to integers
meso2$Size <- gsub("Intermediate","2Intermediate",meso2$Size)  
meso2$Size <- gsub("Small","1Small",meso2$Size) 
meso2$Size <- gsub("Large","3Large",meso2$Size) 
meso2$Size <- as.integer(as.factor(meso2$Size))

## Egg mass distribution
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 
buffer <- 0.3
col <- "lightblue"
den <- density(meso2$Eggs)
xlim <- max(den$x);xlim
ylim <- max(den$y);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)),
     ylim=c(0,ylim+(ylim*buffer)),
     xlab="Number of egg masses",
     ylab="Density",
     main=paste0("Distribution of number of egg masses over ",max(meso1$Week)," weeks"))
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(meso2$Eggs),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean

### N/P concentration v egg mass
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 
with(meso2,stripchart(Eggs~NP,
                      method="jitter", jitter=0.1,
                      pch=20,
                      cex=cex_diam,
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

### N/P concentration v phyto
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
title(ylab="Phytoplankton concentration",line=3.5)

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

# make phyto and peri vs NP into grouped bargraph
col1 <- "lightblue"
col2 <- "orange"
buffer <- 0.3
den <- density(meso2$Phyto_F)
xlim <- max(den$x);xlim
ylim <- max(den$y);ylim
plot(den,
     col=adjustcolor(col1,alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)),
     ylim=c(0,ylim+(ylim*buffer)),
     xlab="",
     ylab="",
     main=""
) 
polygon(den, col=adjustcolor(col1,alpha=0.5),border=col1) # fill AUC 
abline(v=mean(meso2$Phyto_F),col=col1,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
par(new=T) # add periphyton concentration
den2 <- density(meso2$Peri_F)
plot(den2,
     col=adjustcolor(col2,alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)), # uses xy lims from phyto
     ylim=c(0,ylim+(ylim*buffer)),
     xlab="",
     ylab="",
     main=""
     )
polygon(den2, col=adjustcolor(col2,alpha=0.5),border=col2) # fill AUC 
abline(v=mean(meso2$Peri_F),col=col2,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
title(main=paste0("Resource concentration over ",max(meso1$Week)," weeks"),
      xlab="Resource concentration")
title(ylab="Density",line=3.5)
legend("topright",legend=c("Phytoplankton","Periphyton"),title="Resource type",
       border="white",pch=19,ncol=1,bty="n",
       cex=0.75,
       xjust=0.5,yjust=0.5,x.intersp = 0.5,y.intersp = 0.5,
       col=c(col1,col2)
       )

# N/P concentration v presence of schisto
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 
head(meso2)

# egg mass over time v presence of schisto. overlay phyto and pero conc as density polygon(?) 
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 

par(new=T)
points(x=c(0,2,4,6),y=rep(30,4),pch="~",cex=1.5,col="red")# add inoculation points


# egg mass v phyto
# _______________________________________________ compare un/infected snails 

# egg mass v peri
# _______________________________________________ compare un/infected snails 

# diameter dists for different NP levels
# _______________________________________________ compare un/infected snails 
# https://www.statmethods.net/graphs/density.html

# diameter vs phyto
# _______________________________________________ compare un/infected snails 

# diameter vs peri
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 

# Diameter vs egg mass (with schisto)
# _______________________________________________ compare un/infected snails 
plot(meso2$Eggs~meso1$Diameter)
length(meso1$Diameter)/length(meso2$Eggs)
head(meso2)

# joyplot of egg mass over time for each tank/infected tank/ ... 
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 

par(new=T)
points(x=c(0,2,4,6),y=rep(30,4),pch="~",cex=1.5,col="red")# add inoculation points


###########################################################################################

# After sourcing above functions and codes
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


