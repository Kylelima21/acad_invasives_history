#Plotting of Invasives work March 16,2019

library(plyr)
library(ggplot2)
#library(scales)
#library(zoo)

setwd("/Users/Nick/Documents/Projects/ACAD_Invasives/ANP_Invasives_History_Paper/Rwork")

T1<-read.csv('SummaryData.csv', header=TRUE)
names(T1)
head(T1)

#Default margins
#par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

#################################################################
#################################################################
#############  4 plots Sites Species Hours Herbicide ############
#################################################################
#################################################################

##################################################
tiff(filename = "SitesSpeciesHoursHerbicide.tiff",
     width=7.0, height=4.5, units="in", res = 150)

par(mfrow=c(2,2), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,1.0,1.5), oma=c(0.1,0.1,0.1,0.1))

plot(T1$Year, T1$Species,
     pch=16,
     xlab="", ylab="Species (no.)")
lines(T1$Year, T1$Species)
mtext("a)", adj=-0.15, line=0)


plot(T1$Year, T1$Sites,
     pch=16,
     xlab="", ylab="Sites (no.)")
lines(T1$Year, T1$Sites)
mtext("b)", adj=-0.15, line=0)


plot(T1$Year, T1$Hours,
     pch=16,
     xlab="Year", ylab="Hours")
lines(T1$Year, T1$Hours)
mtext("c)", adj=-0.15, line=0)


plot(T1$Year, T1$Herbicide,
     pch=16,
     xlab="Year", ylab="Herbicide (oz)")
lines(T1$Year, T1$Herbicide)
mtext("d)", adj=-0.15, line=0)

dev.off()
#######################################################
#######################################################
#######################################################
par(mfrow=c(1,1))
###Create hours per site

T1$HoursSite<-T1$Hours/T1$Sites

plot(T1$Year, T1$HoursSite,
     pch=16,
     xlab="Year", ylab="Hour/Site")
lines(T1$Year, T1$HoursSite)
mtext("a)", adj=-0.15, line=0)

###Create herbicide per site

T1$HerbSite<-T1$Herbicide/T1$Sites

plot(T1$Year, T1$HerbSite,
     pch=16,
     xlab="Year", ylab="Herbicide (oz)/Site")
lines(T1$Year, T1$HerbSite)
mtext("a)", adj=-0.15, line=0)


#######################################################
#######################################################
########### Species and Sites by Year ################
#######################################################
#######################################################
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T1$Year, T1$Species,
     pch=16,
     xlab="Year", ylab="Species (no.)")
lines(T1$Year, T1$Species)
par(new = T)
plot(T1$Year, T1$Sites,
     pch=1, col='red', 
     axes=F, xlab=NA, ylab=NA)
axis(side=4, col='red', col.axis='red')
mtext(side = 4, line = 1.6, "Sites (no.)", col='red')
lines(T1$Year, T1$Sites, col='red')
#######################################################
#######################################################
#######################################################


#######################################################
#######################################################
########### Herbicide and Hours by Year ################
#######################################################
#######################################################
par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T1$Year, T1$Hours,
     pch=16,
     xlab="Year", ylab="Hours")
lines(T1$Year, T1$Hours)
par(new = T)
plot(T1$Year, T1$Herbicide,
     pch=1, col='red', 
     axes=F, xlab=NA, ylab=NA)
axis(side=4, col='red', col.axis='red')
mtext(side = 4, line = 1.6, "Herbicide (oz)", col='red')
lines(T1$Year, T1$Herbicide, col='red')
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################





S1<-read.csv('SpeciesData.csv', header=TRUE)
head(S1)
names(S1)
#Subsetting 9 Species
S2<-S1[S1$Species %in% c("ALLPET", "BERTHU", "CELORB",
                         "CIRSPP", "FALJAP", "FRAALN",
                         "HERMAN", "LONSPP", "LYTSAL"), ]

#single panel figure with 
####################################################
ggplot(S2, aes(Year, Hours, group=ScientificName),
       stat="identity")+
  geom_point()+
  geom_line()+ 
  labs(x = "Year", y = "Hours")+
  facet_wrap(~ScientificName, ncol=3)+ #, scales = "free_y")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black")) 

ggsave("SpeciesHours20190802.png", device = "png",
       width = 6, height = 4, units = "in")

ggplot(S2, aes(Year, Herbicide, group=ScientificName),
       stat="identity")+
  geom_point()+
  geom_line()+
  labs(x = "Year", y = "Herbicide (oz)")+
  facet_wrap(~ScientificName, ncol=3)+ #, scales = "free_y")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black")) 

ggsave("SpeciesHerbicide20190802.png", device = "png",
       width = 6, height = 4, units = "in")
#######################################################

#########################################
