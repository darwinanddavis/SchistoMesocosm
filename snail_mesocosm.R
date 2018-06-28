### schisto mesocosm ###
### NB: Date and Snail cols contain unnatural values ###

# 27-6-18
# meso2 egg mass data uses only (Eggs > 0), which excludes presence of schisto

#25-6-18
# added mass to meso1
# potential outlier for body mass (1239.301 mg)?

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
# plot overall and un/infected plots as stacked plots in one plot view 
    # layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
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
meso2 <- read.table(f2,header=T,sep=",",row.names=NULL,stringsAsFactors=FALSE, strip.white=TRUE) # read data
colnames(meso2)[2] <- "NP" # fix col names
meso1[is.na(meso1)] <- 0 ; meso2[is.na(meso2)] <- 0 # remove NAs
mass <- 0.0096*(meso1$Diameter^3) # add mass to df
meso1$Mass <- mass  

####### outlier ########
print("Outlier"); meso1[which(meso1$Mass==max(meso1$Mass)),]
outlier <- 0 # remove outlier from data?
if(outlier==1){meso1 <- subset(meso1,Mass<max(Mass))}

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
plot_it(0,"blue","YlOrRd",1,"HersheySans") # set col function params

# get only infected snails
meso1_II <- subset(meso1,subset=Cercariae>0);length(meso1_II$Tank)
meso1_UU <- subset(meso1,subset=Cercariae==0);length(meso1_UU$Tank) 

# set colors you want
col <- "lightblue" 
col2 <- "orange" 
##############################################################################

## Snail diameter (mm) distribution
buffer <- 0.25
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

### un/infected diameter ###
den <- density(meso1_UU$Diameter)
xlim <- max(den$x);xlim; ylim <- max(den$y);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)),
     ylim=c(0,ylim+(ylim*buffer)),
     xlab="Diameter (mm)",
     ylab="Density",
     main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks"))
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(meso1_UU$Diameter),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
par(new=T)
den2 <- density(meso1_II$Diameter) # infected
plot(den2,
     col=adjustcolor(col2,alpha=0.5),
     xlim=c(0,xlim+(xlim*buffer)),
     ylim=c(0,ylim+(ylim*buffer)),
     xlab="",
     ylab="",
     main="")
polygon(den2, col=adjustcolor(col2,alpha=0.5),border=col2) # fill AUC 
abline(v=mean(meso1_II$Diameter),col=col2,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean

### Snail size over time (weeks)
# Shell diameter (mm) over time (weeks)
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # plot stacked plots
ylim <- round_any(max(meso1$Diameter),10,ceiling);ylim
boxplot(Diameter~Week, data=meso1,
        # xlim=c(0,max(meso1$Week)),
        ylim=c(0,ylim),
        col = col,
        notch = T,xlab="Week",ylab="Diameter (mm)",
        main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks"),
        xaxs = "i", yaxs = "i"
        )
abline(h=mean(meso1$Diameter),col=col,lty=3)
par(new=T)
points(x=c(1,3,5,7),y=rep(ylim/2,4),pch="~",col="red")# add inoculation points

### un/infected ###
boxplot(Diameter~Week, data=meso1_UU,
        # xlim=c(0,max(meso1$Week)),
        ylim=c(0,ylim),
        col = col,
        notch = T,xlab="Week",ylab="Diameter (mm)",
        main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks \n(uninfected snails)"),
        xaxs = "i", yaxs = "i"
)
abline(h=mean(meso1_UU$Diameter),col=col,lty=3)
par(new=T)
points(x=c(1,3,5,7),y=rep(ylim/2,4),pch="~",col="red")# add inoculation points
# infected
boxplot(Diameter~Week, data=meso1_II,
        # xlim=c(0,max(meso1$Week)),
        ylim=c(0,ylim),
        col = col2,
        notch = T,xlab="Week",ylab="Diameter (mm)",
        main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks \n (infected snails)"),
        xaxs = "i", yaxs = "i"
)
abline(h=mean(meso1_II$Diameter),col=col,lty=3)
par(new=T)
points(x=c(1,3),y=rep(ylim/2,2),pch="~",col="red")# add inoculation points

### Body mass (mg) over time (weeks) (Soft tissue dry mass in mg = 0.0096 * Diameter[in mm]^3)
### ~1000 eggs inoculated at 0,2,4,6 weeks
ylim <- round_any(max(meso1$Mass),100,ceiling);ylim
boxplot(Mass~Week, data=meso1,
        # xlim=c(0,max(meso1$Week)),
        ylim=c(0,ylim),
        col = col,
        notch = T,xlab="Week",ylab="Dry body mass (mg)",
        main=paste0("Body mass (mg) over ",max(meso1$Week)," weeks"),
        xaxs = "i", yaxs = "i"
)
abline(h=mean(meso1$Mass),col=col,lty=3)
points(x=c(1,3,5,7),y=rep(ylim/3,4),pch="~",col="red")# add inoculation points

### uninfected ###
boxplot(Mass~Week, data=meso1_UU,
        # xlim=c(0,max(meso1$Week)),
        ylim=c(0,ylim),
        col = col,
        notch = T,xlab="Week",ylab="Dry body mass (mg)",
        main=paste0("Body mass (mg) over ",max(meso1$Week)," weeks \n(uninfected)"),
        xaxs = "i", yaxs = "i"
)
abline(h=mean(meso1$Mass),col=col,lty=3)
points(x=c(1,3,5,7),y=rep(ylim/3,4),pch="~",col="red")# add inoculation points

### infected ###
ylim <- round_any(max(meso1_II$Mass),100,ceiling);ylim
boxplot(Mass~Week, data=meso1_II,
        # xlim=c(0,max(meso1$Week)),
        ylim=c(0,ylim),
        col = col2,
        notch = T,xlab="Week",ylab="Dry body mass (mg)",
        main=paste0("Body mass (mg) over ",max(meso1$Week)," weeks \n(uninfected)"),
        xaxs = "i", yaxs = "i"
)
abline(h=mean(meso1$Mass),col=col2,lty=3)
points(x=c(1,3),y=rep(ylim,2),pch="~",col="red")# add inoculation points

### Snail size per tank 
# Shell diameter (mm) per tank
# _______________________________________________ compare un/infected snails 
par(mfrow=c(1,1))
ylim <- round_any(max(meso1$Diameter),10,ceiling)
boxplot(Diameter~Tank, data=meso1,
        ylim=c(0,ylim),
        col = "light blue",
        notch = T,xlab="Tank",ylab="Diameter (mm)",
        main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks")
)
abline(h=mean(meso1$Diameter),col="pink",lty=3)
with(meso1,t.test(Diameter,Tank)) # t.test

### Snail size and number of cercariae produced
# Point size by cercariae number
with(meso1,plot(Diameter,Cercariae,pch=20,
                col=adjustcolor(col,alpha=0.5),
                cex=cex_cer,
                ylab="Number of cercariae released over 90 mins",xlab="Diameter (mm)",
                ))
title("Number of cercarie for each snail length (mm)")
abline(v=mean(meso1$Diameter),lty=3,col=col)# mean diameter
outer <- meso1[which(meso1$Mass==max(meso1$Mass)),][,c("Diameter","Cercariae")] # identify outlier 
points(outer,col="red",pch=20) # plot outlier
# legend(c(50,5500),legend=paste0(unique(meso1$Sampling_Effort)," samples"),
#        title ="Sampling effort",
#        border="white",pch=20,col=brewer.pal(meso1$Sampling_Effort,"Blues")[1:3],
#        pt.cex=unique(meso1$Sampling_Effort),
#        bty="n",
#        # xjust=1,
#        adj=-2,
#        # y.intersp=0.5,
#        text.font=NULL)

### Snail mass and cercariae produced (mg)
with(meso1,plot(Mass,Cercariae,pch=20,
                col=adjustcolor(col,alpha=0.5),
                cex=cex_cer,
                ylab="Number of cercariae released over 90 mins",xlab="Dry body mass (mg)",
))
title("Number of cercariae for each snail mass (mg)")
abline(v=mean(meso1$Mass),lty=3,col=col)# mean diameter
outer <- meso1[which(meso1$Mass==max(meso1$Mass)),][,c("Mass","Cercariae")] # identify outlier 
points(outer,col="red",pch=20) # plot outlier      


### Snail size per tank
# Shell diameter (mm) 
# _______________________________________________ compare un/infected snails 
# @@@ joyplot
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # plot stacked plots

tank <- 2 # select tank #. max 48

snail <- subset(meso1,subset=Tank==tank) # get tank level indiviudals
diam_total <- 1 # set ylim either to max for tank or max across all tanks (16.8) 
den <- density(snail$Diameter) # get diameter density
xlim <- round_any(max(den$x),10,ceiling);xlim
ylim <- round_any(max(den$y),0.05,ceiling);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim),ylim=c(0,ylim),
     xlab="",ylab="",main=""
)
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(snail$Diameter),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
title(main=paste0("Shell diameter (mm) distribution for tank #",tank),
      xlab="Shell diameter (mm)")
title(ylab="Density",line=3.5)

### uninfected ###
snail_UU <- subset(snail,subset=Cercariae==0)
den <- density(snail_UU$Diameter) # get diameter density
xlim <- round_any(max(den$x),10,ceiling);xlim
ylim <- round_any(max(den$y),0.05,ceiling);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim),ylim=c(0,ylim),
     xlab="",ylab="",main=""
)
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(snail_UU$Diameter),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
title(main=paste0("Uninfected snails in tank #",tank),
      xlab="Shell diameter (mm)")
title(ylab="Density",line=3.5)

### infected ###
snail_II <- subset(snail,subset=Cercariae>0)
if(length(snail_II$Tank)>0){
  den2 <- density(snail_II$Diameter) # get diameter density
  plot(den2,
       col=adjustcolor(col2,alpha=0.5),
       xlim=c(0,xlim),ylim=c(0,ylim),
       xlab="",ylab="",main=""
  )
  polygon(den2, col=adjustcolor(col2,alpha=0.5),border=col2) # fill AUC 
  abline(v=mean(snail_II$Diameter),col=col2,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
  title(main=paste0("Infected snails in tank #",tank),
        xlab="Shell diameter (mm)")
  }else{
    plot(0,0,type="n");title(main=paste0("Infected snails in tank #",tank)); text(0,0.5,paste0("No cercariae \nin tank #",tank))
  }

### Individual cercariae production over time
# Cercariae shed over 90 mins per week
### ~1000 eggs inoculated at 0,2,4,6 weeks
tank <- 21 # max 48 
cer_total <- 0 # set ylim either to max for tank or max across all tanks (6100) 

snail <- subset(meso1,subset=Tank==tank) # get tank level indiviudals
snail <- subset(snail,subset=Cercariae>0) # get only cercariae
buffer <- 0.25 # axis buffer
xlim <- max(meso1$Week) # uses total num of weeks
ifelse(cer_total==1,ylim <- round_any(max(meso1$Cercariae),100),ylim <- max(snail$Cercariae))
par(mfrow=c(1,1))
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
  points(x=c(0,2,4,6),y=rep(max(snail$Cercariae)/3,4),pch="~",cex=1.5,col="red")# add inoculation points
}else{print(paste0("No cercariae in tank #",tank))}

############################################################################################################

# Mesocosm 2 data sheet
meso2$Schisto <- as.integer(as.factor(meso2$Schisto))-1# convert Y/N in Schisto col to 1/0

### get snails with egg masses
#### First get presence of schisto
meso2_UU <-  subset(meso2,Schisto==0)
meso2_II <- subset(meso2,Schisto==1)

# get uninfected snails with egg masses 
eggs_UU <- subset(meso2_UU,Eggs>0)
# get infected snails with egg masses
eggs_II <- subset(meso2_II,Eggs>0)

# get size classes 
# convert size to integers
meso2$Size <- gsub("Intermediate","2Intermediate",meso2$Size)  
meso2$Size <- gsub("Small","1Small",meso2$Size) 
meso2$Size <- gsub("Large","3Large",meso2$Size) 
meso2$Size <- as.integer(as.factor(meso2$Size))
small <- subset(meso2,Size==1) #small
int <- subset(meso2,Size==2) #intermediate
large <- subset(meso2,Size==3) #large

# get NP conc
high <- subset(meso2,NP=="High") # high NP conc
low <- subset(meso2,NP=="Low") # low NP conc

## Egg mass distribution
# _______________________________________________ compare un/infected snails 
buffer <- 0.3
den <- density(meso2$Eggs[meso2$Eggs>0]) # get only snails with eggs
xlim <- round_any(max(den$x),50,ceiling);xlim
ylim <- round_any(max(den$y),0.01,ceiling);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim),
     ylim=c(0,ylim),
     xlab="Number of egg masses",
     ylab="Density",
     main=paste0("Distribution of number of egg masses over ",max(meso1$Week)," weeks"))
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(meso2$Eggs),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean

### N/P concentration v egg mass
# _______________________________________________ compare un/infected snails 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # plot stacked plots
ylim=round_any(max(meso2$Eggs),10)
with(meso2,stripchart(Eggs~NP,
                      method="jitter", jitter=0.1,
                      pch=20,cex=2,
                      # cex=cex_diam,
                      col=adjustcolor(col,alpha=0.3),
                      vertical=T,
                      ylim=c(0,ylim),
                      group.names=c("High","Low"),
                      xlab="",ylab="",main="")
     )
abline(h=mean(meso2$Eggs),col=col,lty=3)   
title(main=paste0("Number of egg masses for high and low N/P levels over ",max(meso1$Week)," weeks"),
      xlab="N/P level")
title(ylab="Number of egg masses",line=3.5)

### uninfected ###
with(eggs_UU,stripchart(Eggs~NP,
                      method="jitter", jitter=0.1,
                      pch=20,cex=2,
                      col=adjustcolor(col,alpha=0.3),
                      vertical=T,
                      ylim=c(0,ylim),
                      group.names=c("High","Low"),
                      xlab="",ylab="",main="")
)
abline(h=mean(eggs_UU$Eggs),col=col,lty=3)
title(main=paste0("Uninfected snails"),
      xlab="N/P level")
title(ylab="Number of egg masses",line=3.5)

### infected ###
with(eggs_II,stripchart(Eggs~NP,
                        method="jitter", jitter=0.1,
                        pch=20,cex=2,
                        col=adjustcolor(col2,alpha=0.3),
                        vertical=T,
                        ylim=c(0,ylim),
                        group.names=c("High","Low"),
                        xlab="",ylab="",main="")
)
abline(h=mean(eggs_II$Eggs),col=col2,lty=3)
title(main=paste0("Infected snails"),
      xlab="N/P level")

### phyto = flourescence units
### peri = flourescence per 2 weeks / 3.5 inch^2 tile (gross productivity biomass rate)

### phyto and peri distribution 
par(mfrow=c(1,1))
den <- density(meso2$Phyto_F)
xlim <- round_any(max(den$x),10000,ceiling);xlim
ylim <- round_any(max(den$y),0.0001,ceiling);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim),
     ylim=c(0,ylim),
     xlab="",ylab="",main=""
) 
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(meso2$Phyto_F),col=col,lty=3,ylim=c(0,ylim+(ylim*buffer))) # get mean
par(new=T) # add periphyton concentration
den2 <- density(meso2$Peri_F)
plot(den2,
     col=adjustcolor(col2,alpha=0.5),
     xlim=c(0,xlim), # uses xy lims from phyto
     ylim=c(0,ylim),
     xlab="",ylab="",main=""
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

# egg mass over time v presence of schisto. inset phyto and pero conc as density polygon(?) 
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # plot stacked plots
par(mar=c(3.5,5,2,2))
xlim <- max(meso2$Week)
ylim <- round_any(max(meso2$Eggs),10)
with(meso2,stripchart(Eggs~Week,
                      method="jitter", jitter=0.1,
                      pch=20,cex=2,
                      col=adjustcolor(col,alpha=0.5),
                      vertical=T,
                      xlim=c(0,xlim), ylim=c(0,ylim),
                      group.names=unique(meso2$Week),
                      xlab="Weeks",
                      ylab="Number of egg masses",
                      main=paste0("Number of egg masses over ",xlim," weeks")
                      ))
abline(h=mean(meso2$Eggs),col=col,lty=3) 
par(new=T)
points(x=c(0,2,4,6),y=rep(max(ylim)/3,4),pch="~",cex=1,col="red")# add inoculation points
axis(1,at=c(0,xlim),labels=c("0",""))# bookending axis tick marks

### uninfected ###
with(eggs_UU, stripchart(Eggs~Week,
           method="jitter", jitter=0.1,
           pch=20,cex=2,
           col=adjustcolor(col,alpha=0.3),
           vertical=T,
           xlim=c(0,xlim),ylim=c(0,ylim),
           group.names=unique(eggs_UU$Week),
           xlab="Weeks",
           ylab="Number of egg masses",
           main="Uninfected")
)
par(new=T)
points(x=c(0,2,4,6),y=rep(max(ylim)/3,4),pch="~",cex=1,col="red")# add inoculation points
axis(1,at=c(0,xlim),labels=c("0","16"))# bookending axis tick marks

### infected ###
with(eggs_II, stripchart(Eggs~Week,
                         method="jitter", jitter=0.1,
                         pch=20,cex=2,
                         col=adjustcolor(col2,alpha=0.3),
                         vertical=T,
                         xlim=c(0,xlim),ylim=c(0,ylim),
                         group.names=unique(eggs_II$Week),
                         xlab="Weeks",
                         ylab="Number of egg masses",
                         main="Infected")
)
par(new=T)
points(x=c(0,2,4,6),y=rep(max(ylim)/3,4),pch="~",cex=1,col="red")# add inoculation points
axis(1,at=c(0,xlim),labels=c("0","16"))# bookending axis tick marks

# size class vs peri
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 

# size class vs egg mass (with schisto)
# _______________________________________________ compare un/infected snails 

#### Egg masses > 0  
den <- density(small$Eggs[small$Eggs>0])
den2 <- density(int$Eggs[int$Eggs>0])
den3 <- density(large$Eggs[large$Eggs>0])
xlim <- round_any(max(den2$x),100) #den2 xlim
ylim <- round_any(max(den2$y),0.01,ceiling);ylim # den2 ylim

colvec <- c(4,6,9) # index for colfunc color palette in plot_it function 
plot(den,
     col=adjustcolor(colfunc[colvec[1]],alpha=0.5),
     xlim=c(0,xlim),
     ylim=c(0,ylim),
     xlab="Number of egg masses",
     ylab="Density",
     main=""
     )
lines(den2,col=colfunc[colvec[2]])
lines(den3,col=colfunc[colvec[3]])

polygon(den, col=adjustcolor(colfunc[colvec[1]],alpha=0.5),border=colfunc[colvec[1]]) # fill AUC 
polygon(den2, col=adjustcolor(colfunc[colvec[2]],alpha=0.5),border=colfunc[colvec[2]]) # fill AUC 
polygon(den3, col=adjustcolor(colfunc[colvec[3]],alpha=0.5),border=colfunc[colvec[3]]) # fill AUC 

abline(v=mean(small$Eggs),col=adjustcolor(colfunc[colvec[1]]),lty=3,ylim=c(0,ylim)) # get mean
abline(v=mean(int$Eggs),col=adjustcolor(colfunc[colvec[2]]),lty=3,ylim=c(0,ylim)) # get mean
abline(v=mean(large$Eggs),col=adjustcolor(colfunc[colvec[3]]),lty=3,ylim=c(0,ylim)) # get mean


with(small,plot(Eggs~Phyto_F,ylim=c(0,200)))
with(int,plot(Eggs~Phyto_F,ylim=c(0,200)))
with(large,plot(Eggs~Phyto_F,ylim=c(0,200)))


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

############### Lab ##################

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
                      main=paste0("Periphyton concentration for high and low N/P \n levels over ",max(meso1$Week)," weeks")
))
abline(h=mean(meso2$Eggs),col="pink",lty=3)

