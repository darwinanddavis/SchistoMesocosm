### schisto mesocosm ###
### NB: Date and Snail cols contain unnatural values ###

# 19-7-18
# begin summary stats

# 17-7-18
# changed package installation for Rmd 
# chars to numeric for t.test 

#10-7-18
# fixed package installation for Rmd file using cache

#5-7-18
# fixed legends

#28-6-18
# plot_it_gg() function for ggplot global params
# ggridges for egg masses per week/tank

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
# provide plot options for diameter and mass without outlier 
# Try population Pyramid plot for infected v uninfected /Users/malishev/Documents/Melbourne Uni/Programs/R code
# correlation coefficients between body mass (tissue) and cerc produced, periphyton consumed, diameter      
# turn main() function into PDF markdown output

##### install dependencies
packages <- c("rgdal","dplyr","zoo","RColorBrewer","viridis","plyr","digitize","jpeg","devtools","imager","dplyr","ggplot2","ggridges","ggjoy","ggthemes","svDialogs","data.table","tibble","extrafont","sp")   
if (require(packages)) {
  install.packages(packages,dependencies = T)
  require(packages)
}
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
meso2$Week <- as.numeric(meso2$Week)
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

### Setting ggplot theme graphics
plot_it_gg <- function(bg){ # bg = colour to plot bg, family = font family
  if(bg=="black"){
    colvec<-magma(200,1)
    bg <- colvec[1]
  }else{
    colvec<-bpy.colors(200)
    bg = colvec[1]
  }
  theme_tufte(base_family = "HersheySans") +
    theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = bg,colour = bg),plot.background = element_rect(fill=bg)) +
    theme(axis.line = element_line(color = "white")) +theme(axis.ticks = element_line(color = "white")) +theme(plot.title = element_text(colour = "white")) +theme(axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white")) +theme(axis.text.x = element_text(color = "white"),axis.text.y = element_text(color = "white")) +theme(legend.key = element_rect(fill = bg)) + theme(legend.title = element_text(colour="white")) + theme(legend.text = element_text(colour="white")) 
}

print("1/0, set colour, set colour palette 'display.brewer.all()',set alpha for col,set font")
plot_it(0,"blue","YlOrRd",1,"HersheySans") # set col function params
plot_it_gg("blue") # same as above

# set colors you want
col <- "lightblue" 
col2 <- "orange" 

# get only infected snails
meso1_II <- subset(meso1,subset=Cercariae>0);length(meso1_II$Tank)
meso1_UU <- subset(meso1,subset=Cercariae==0);length(meso1_UU$Tank) 


##############################################################################
## Mesocosm 1 data  
### Snail diameter (mm) distribution
den <- density(meso1$Diameter)
xlim <- round_any(max(den$x),10,ceiling);xlim; ylim <- round_any(max(den$y),0.1,ceiling);ylim
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim),
     ylim=c(0,ylim),
     xlab="Diameter (mm)",
     ylab="Density",
     main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks"))
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(meso1$Diameter),col=col,lty=3,ylim=c(0,ylim)) # get mean

## un/infected diameter 
# Uninfected
den <- density(meso1_UU$Diameter)
plot(den,
     col=adjustcolor(col,alpha=0.5),
     xlim=c(0,xlim),
     ylim=c(0,ylim),
     xlab="Diameter (mm)",
     ylab="Density",
     main=paste0("Shell diameter (mm) over ",max(meso1$Week)," weeks"))
polygon(den, col=adjustcolor(col,alpha=0.5),border=col) # fill AUC 
abline(v=mean(meso1_UU$Diameter),col=col,lty=3,ylim=c(0,ylim)) # get mean
par(new=T)
# Infected 
den2 <- density(meso1_II$Diameter) 
plot(den2,
     col=adjustcolor(col2,alpha=0.5),
     xlim=c(0,xlim),
     ylim=c(0,ylim),
     xlab="",
     ylab="",
     main="")
polygon(den2, col=adjustcolor(col2,alpha=0.5),border=col2) # fill AUC 
abline(v=mean(meso1_II$Diameter),col=col2,lty=3,ylim=c(0,ylim)) # get mean
par(family="mono")
legend("right",legend=c("Uninfected","Infected"),col=c(col,col2),
       bty="n",pch=20,pt.cex=1.5,cex=0.7,y.intersp = 0.5, xjust = 0.5,
       title="",title.adj = 0.3,
       # text.font=2,
       trace=T,inset=0.1)

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

### Body mass (mg) over time (weeks) 
# Soft tissue dry mass in mg = 0.0096 * Diameter[in mm]^3)
### ~1000 eggs inoculated at 0,2,4,6 weeks
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # plot stacked plots
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
        notch = F,xlab="Week",ylab="Dry body mass (mg)",
        main=paste0("Body mass (mg) over ",max(meso1$Week)," weeks \n(uninfected)"),
        xaxs = "i", yaxs = "i"
)
abline(h=mean(meso1$Mass),col=col2,lty=3)
points(x=c(1,3),y=rep(ylim/1.2,2),pch="~",col="red")# add inoculation points

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
with(meso1,plot(Diameter,log(Cercariae),pch=20,
                col=adjustcolor(col,alpha=0.5),
                cex=cex_cer+0.5,
                ylab="Log number of cercariae released over 90 mins",xlab=" Diameter (mm)"
                ))
title("Number of cercarie for each snail length (mm)")
abline(v=mean(meso1$Diameter),lty=3,col=col)# mean diameter
outer <- meso1[which(meso1$Diameter==max(meso1$Diameter)),][,c("Diameter","Cercariae")]; outer # identify outlier 
points(outer,col="red",pch=20) # plot outlier
# Linear log
# summary(with(meso1,lm(log(Cercariae)~Diameter))) # linear log

### Snail mass and cercariae produced (mg)
with(meso1,plot(log(Mass),Cercariae,pch=20,
                col=adjustcolor(col,alpha=0.5),
                cex=cex_cer+0.5,
                # cex=1.5,
                ylab="Number of cercariae released over 90 mins",xlab="Log dry body mass (mg)",
))
title("Number of cercariae for each log snail mass (mg)")
abline(v=log(mean(meso1$Mass)),lty=3,col=col)# mean diameter
outer <- meso1[which(meso1$Mass==max(meso1$Mass)),][,c("Mass","Cercariae")] # identify outlier 
points(outer,col="red",pch=20) # plot outlier      

### Snail size per tank
# Shell diameter (mm) 
# _______________________________________________ compare un/infected snails 
# Select tank #. max 48
tank <- 2 

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # plot stacked plots
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
abline(v=mean(snail$Diameter),col=col,lty=3,ylim=c(0,ylim)) # get mean
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
abline(v=mean(snail_UU$Diameter),col=col,lty=3,ylim=c(0,ylim)) # get mean
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
  abline(v=mean(snail_II$Diameter),col=col2,lty=3,ylim=c(0,ylim)) # get mean
  title(main=paste0("Infected snails in tank #",tank),
        xlab="Shell diameter (mm)")
  }else{
    plot(0,0,type="n");title(main=paste0("Infected snails in tank #",tank)); text(0,0.5,paste0("No cercariae \nin tank #",tank))
  }

### Cercariae production over time
# Cercariae shed over 90 mins per week
### ~1000 eggs inoculated at 0,2,4,6 weeks
## Snail abundance over time (weeks)
par(mfrow=c(1,1))
xlim <- max(meso1$Week) # uses total num of weeks
ylim <- round_any(max(meso1$Cercariae),100,ceiling);ylim
with(meso1,plot(Cercariae~Week,
                col=adjustcolor(col,alpha=0.5),
                type="h",
                lwd=5,
                xlim=c(0,xlim),ylim=c(0,ylim),
                xlab="",ylab="",main=""
))
abline(h=mean(meso1$Cercariae),col=col,lty=3,ylim=c(0,ylim)) # get mean
title(main=paste0("Cercariae production over ",max(meso1$Week)," weeks"),
      xlab="Week")
title(ylab="Number of cercariae shed in 90 mins",line=3.5)

### Tank cercariae production over time per tank
tank <- 9 # max 48 

cer_total <- 0 # set ylim either to max for tank (1) or max across all tanks (6100) 
snail <- subset(meso1,subset=Tank==tank) # get tank level individuals
snail <- subset(snail,subset=Cercariae>0) # get only cercariae
xlim <- max(meso1$Week) # uses total num of weeks
ylim <- round_any(max(snail$Cercariae),100,ceiling);ylim
ifelse(cer_total==1,ylim <- round_any(max(meso1$Cercariae),100,ceiling),ylim <- round_any(max(snail$Cercariae),100,ceiling))
par(mfrow=c(1,1))
if(length(snail$Cercariae)>0){
  with(snail,plot(Cercariae~Week,
                  col=adjustcolor(col,alpha=0.5),
                  type="h",
                  lwd=5,
                  xlim=c(0,xlim),ylim=c(0,ylim),
                  xlab="",ylab="",main=""
  ))
  abline(h=mean(snail$Cercariae),col=col,lty=3,ylim=c(0,ylim)) # get mean
  title(main=paste0("Cercariae production for tank ",tank," over ",max(meso1$Week)," weeks"),
        xlab="Week")
  title(ylab="Number of cercariae shed in 90 mins",line=3.5)
  par(new=T)
  points(x=c(0,2,4,6),y=rep(max(snail$Cercariae)/3,4),pch="~",cex=1.5,col="red")# add inoculation points
}else{print(paste0("No cercariae in tank #",tank))}

#####   
############################################################################################################
############################################################################################################
############################################################################################################

# Mesocosm 2 data sheet  

meso2$Schisto <- as.integer(as.factor(meso2$Schisto))-1# convert Y/N in Schisto col to 1/0
# convert size to integers
meso2$Size <- gsub("Intermediate","2Intermediate",meso2$Size)  
meso2$Size <- gsub("Small","1Small",meso2$Size) 
meso2$Size <- gsub("Large","3Large",meso2$Size) 
meso2$Size <- as.integer(as.factor(meso2$Size))

### get snails with egg masses
#### First get presence of schisto
meso2_UU <-  subset(meso2,Schisto==0)
meso2_II <- subset(meso2,Schisto==1)

# get uninfected snails with egg masses 
eggs_UU <- subset(meso2_UU,Eggs>0)
# get infected snails with egg masses
eggs_II <- subset(meso2_II,Eggs>0)

# get size classes 
small <- subset(meso2,Size==1) #small
int <- subset(meso2,Size==2) #intermediate
large <- subset(meso2,Size==3) #large

# get NP conc
high <- subset(meso2,NP=="High") # high NP conc
low <- subset(meso2,NP=="Low") # low NP conc

### Egg mass distribution
# _______________________________________________ compare un/infected snails 
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
abline(v=mean(meso2$Eggs),col=col,lty=3,ylim=c(0,ylim)) # get mean

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
with(eggs_UU,t.test(Eggs,as.integer(as.factor(NP))))

### phyto and peri distribution 
### phyto = flourescence units
### peri = flourescence per 2 weeks / 3.5 inch^2 tile (gross productivity biomass rate)
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
abline(v=mean(meso2$Phyto_F),col=col,lty=3,ylim=c(0,ylim)) # get mean
par(new=T) # add periphyton concentration
den2 <- density(meso2$Peri_F)
plot(den2,
     col=adjustcolor(col2,alpha=0.5),
     xlim=c(0,xlim), # uses xy lims from phyto
     ylim=c(0,ylim),
     xlab="",ylab="",main=""
     )
polygon(den2, col=adjustcolor(col2,alpha=0.5),border=col2) # fill AUC 
abline(v=mean(meso2$Peri_F),col=col2,lty=3,ylim=c(0,ylim)) # get mean
title(main=paste0("Resource concentration over ",max(meso1$Week)," weeks"),
      xlab="Resource concentration")
title(ylab="Density",line=3.5)
legend("topright",legend=c("Phytoplankton","Periphyton"),title="Resource type",
       border="white",pch=19,ncol=1,bty="n",
       cex=0.75,
       xjust=0.5,yjust=0.5,x.intersp = 0.5,y.intersp = 0.5,
       col=c(col,col2)
       )

### Egg mass over time v presence of schisto. 
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

### Size class vs peri
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 

# size class vs egg mass (with schisto)
# _______________________________________________ compare un/infected snails 
# small, int, large

#### Egg masses > 0  
den <- density(small$Eggs[small$Eggs>0])
den2 <- density(int$Eggs[int$Eggs>0])
den3 <- density(large$Eggs[large$Eggs>0])
xlim <- round_any(max(den2$x),100) #den2 xlim
ylim <- round_any(max(den2$y),0.01,ceiling);ylim # den2 ylim

colvec <- c(4,6,9) # index for colfunc color palette in plot_it function 
par(mfrow=c(1,1))
plot(den,
     col=adjustcolor(colfunc[colvec[1]],alpha=0.5),
     xlim=c(0,xlim),
     ylim=c(0,ylim),
     # type="h",# fills density
     xlab="Number of egg masses",
     ylab="Density",
     main="Number of egg masses for each snail size class"
     )
lines(den2,col=adjustcolor(colfunc[colvec[2]])) # den2
lines(den3,col=adjustcolor(colfunc[colvec[3]])) # den3
# fill densities
polygon(den, col=adjustcolor(colfunc[colvec[1]],alpha=0.5),border=colfunc[colvec[1]]) # fill AUC 
polygon(den2, col=adjustcolor(colfunc[colvec[2]],alpha=0.5),border=colfunc[colvec[2]]) # fill AUC 
polygon(den3, col=adjustcolor(colfunc[colvec[3]],alpha=0.5),border=colfunc[colvec[3]]) # fill AUC 
# means
abline(v=mean(small$Eggs),col=adjustcolor(colfunc[colvec[1]]),lty=3,ylim=c(0,ylim)) # get mean
abline(v=mean(int$Eggs),col=adjustcolor(colfunc[colvec[2]]),lty=3,ylim=c(0,ylim)) # get mean
abline(v=mean(large$Eggs),col=adjustcolor(colfunc[colvec[3]]),lty=3,ylim=c(0,ylim)) # get mean

par(family="mono")
legend("right",legend=c("Small","Intermediate","Large"),col=c(colfunc[colvec[1:3]]),
       bty="n",pch=20,pt.cex=1.5,cex=0.7,y.intersp = 0.5, xjust = 0.5,
       title="Snail size class",title.adj = 0.3,text.font=2,
       trace=T,inset=0.1)

### Uninfected  
small_UU <- subset(small,Schisto==0)
int_UU <- subset(int,Schisto==0)
large_UU <- subset(large,Schisto==0)
### Infected
small_II <- subset(small,Schisto==1)
int_II <- subset(int,Schisto==1)
large_II <- subset(large,Schisto==1)

### Egg mass per week 
### ~1000 eggs inoculated at 0,2,4,6 weeks
# _______________________________________________ compare un/infected snails 
# set data to appropriate class
meso2$Eggs <- as.numeric(meso2$Eggs)
meso2$Week <- as.factor(meso2$Week)
d <- meso2

ggplot(meso2, aes(x = Eggs, y = as.factor(Week), fill=..x..)) + # geom_density_ridges()
  # geom_density_ridges_gradient(scale = 2, size=0.25, rel_min_height = 0.01,panel_scaling=T) +# scale = overlap
  # scale_fill_viridis(name = "Eggs", alpha=0.5, option = "magma",direction=-1) + # "magma", "inferno","plasma", "viridis", "cividis"
  geom_density_ridges(scale = 3, size=0.2,color="black", rel_min_height = 0.01,fill=col,alpha=0.5) +
  labs(title = paste0("Number of egg masses per week ")) +
  xlab("Number of egg masses") +
  ylab("Week") +
  # theme_ridges(grid=F,center_axis_labels = T)
  plot_it_gg("blue")

#### uninfected ####
ggplot(meso2_UU, aes(x = Eggs, y = as.factor(Week), fill=..x..)) + # geom_density_ridges()
  # geom_density_ridges_gradient(scale = 2, size=0.25, rel_min_height = 0.01,panel_scaling=T) +# scale = overlap
  # scale_fill_viridis(name = "Eggs", alpha=0.5, option = "magma",direction=-1) + # "magma", "inferno","plasma", "viridis", "cividis"
  geom_density_ridges(scale = 3, size=0.2,color="black", rel_min_height = 0.01,fill=col,alpha=0.5) +
  labs(title = paste0("Number of egg masses per week for uninfected hosts")) +
  xlab("Number of egg masses") +
  ylab("Week") +
  # theme_ridges(grid=F,center_axis_labels = T)
  plot_it_gg("blue")

#### infected ####
ggplot(meso2_II, aes(x = Eggs, y = as.factor(Week), fill=..x..)) + # geom_density_ridges()
  # geom_density_ridges_gradient(scale = 2, size=0.25, rel_min_height = 0.01,panel_scaling=T) +# scale = overlap
  # scale_fill_viridis(name = "Eggs", alpha=0.5, option = "magma",direction=-1) + # "magma", "inferno","plasma", "viridis", "cividis"
  geom_density_ridges(scale = 3, size=0.2,color="black", rel_min_height = 0.01,fill=col2,alpha=0.5) +
  labs(title = paste0("Number of egg masses per week for infected hosts")) +
  xlab("Number of egg masses") +
  ylab("Week") +
  # theme_ridges(grid=F,center_axis_labels = T)
  plot_it_gg("blue")


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

### Diameter per week (minus outlier)
gradient <- 1 # plot with color gradient?
ggplot(subset(meso1,Diameter<max(Diameter)), aes(x = Diameter, y = as.factor(Week), fill=..x..)) + # geom_density_ridges()
  # scale = overlap
  geom_density_ridges_gradient(scale = 5, size=0.2,color="black", rel_min_height = 0.01,panel_scaling=T,alpha=0.2) +
  geom_density_ridges(scale = 5, size=0.2,color="black", rel_min_height = 0.01,fill="white",alpha=0.2) +
  geom_point(aes(x=30,y=c(3)),shape=21) +
  # geom_density_ridges(scale = 5, size=0.2,color="white", rel_min_height = 0.01,fill=col,alpha=0.5) +
  scale_fill_viridis(name = "Diameter", alpha=0.1, option = "magma",direction=-1) + # "magma", "inferno","plasma", "viridis", "cividis"
  labs(title = paste0("Snail diameter over ",max(meso1$Week)," weeks")) +
  xlab("Snail diameter (mm)") +
  ylab("Week") +
  # theme_ridges(grid=F,center_axis_labels = T)
  plot_it_gg("blue")   

# hex and geombin for diameter vs cercariae 
ggplot(meso1,aes(Diameter,Cercariae))+
  geom_hex(color=magma(200)[length(meso1$Diameter)],size=0.5,alpha=0.5) +
  # geom_bin2d(stat = "bin2d",color=col,alpha=0.5, size=0.5,linetype=1) +
  scale_fill_gradientn(limits=c(0,1000), breaks=seq(0,1000, by=500), colours=magma(200)) +
  # theme_ridges(grid=F,center_axis_labels = T) +
  plot_it_gg("blue")

#### single color palette with alpha ####
ggplot(meso2_II, aes(x = Eggs, y = as.factor(Week), fill=..x..)) + # geom_density_ridges()
  geom_joy(data=d, scale = 3, size = 0.25, rel_min_height = 0.01, fill="pink", alpha=0.5) +
  labs(title = paste0("Number of eggs masses per week ")) +xlab("Number of egg masses") +ylab("Week") +theme_ridges(grid=F,center_axis_labels = T)

#### infected ####
ggplot(meso2_II, aes(x = Eggs, y = as.factor(Week), fill=..x..)) + # geom_density_ridges()
  geom_density_ridges_gradient(scale = 2, size=0.25, rel_min_height = 0.01,panel_scaling=T) +# scale = overlap
  scale_fill_viridis(name = "Eggs", alpha=0.5, option = "magma",direction=-1) + # "magma", "inferno","plasma", "viridis", "cividis"
  labs(title = paste0("Number of egg masses per week ")) +
  xlab("Number of egg masses") +
  ylab("Week") +
  # theme_ridges(grid=F,center_axis_labels = T)
  plot_it_gg("blue")

ggplot(meso2_UU, aes(x = Eggs, y = as.factor(Size), fill=..x..)) + # geom_density_ridges()
  geom_density_ridges(aes(x = Eggs, fill = as.factor(Size)), 
                      alpha = 0.5, color = "gray",size=0.25, from = 0, to = 100) +
  labs(title = paste0("Number of egg masses per size class ")) +
  xlab("Number of egg masses") +
  ylab("Size class") +
  # theme_ridges(grid=F,center_axis_labels = T)
  plot_it_gg("blue")


# legends

par(family="mono") # legend
legend <- c(round_any(max(meso1$Cercariae),100,floor),round_any(max(meso1$Cercariae),100,floor)/2,round_any(min(meso1$Cercariae),100,floor));legend
legend("right",legend=c("6000+", "1000-3000","<1000"),col=col,
       bty="n",pch=20,
       pt.cex=legend/1000+0.5,
       y.intersp = 0.5, xjust = 0.5,
       title="No. of cercariae",title.adj = 0.3,
       # text.font=2,
       trace=T,inset=0.3)


