rm(list=ls())

#Needs pitchRx to Run
library(pitchRx)

#Scrape Data
prx_scrape <- scrape(start = "2017-04-02", end = "2017-04-04")
pitches <- prx_scrape$pitch
at_bats <- prx_scrape$atbat

#Solve Quadratic Equation for t of vR
a = pitches$ax^2 + pitches$ay^2 + pitches$az^2
b = 2*(pitches$ax*pitches$vx0+pitches$ay*pitches$vy0+pitches$az*pitches$vz0)
c = pitches$vx0^2 + pitches$vy0^2 + pitches$vz0^2 - (5280*pitches$start_speed/3600)^2
t = (-b-sqrt(b^2-4*a*c))/(2*a)
pitches$yRelease = pitches$y0+pitches$vy0*t+.5*pitches$ay*t^2

#Merge Dataframes
pitches<-merge(pitches,at_bats[,c("num","gameday_link","pitcher_name")],by=c("num","gameday_link"),all.x=TRUE)

#Calculate Extension
pitches$Extension <- 60.5 - pitches$yRelease

#Create an Aggregated Table, if you want one
release_distance <- aggregate(yRelease~pitcher_name,data=pitches,FUN=mean)
release_distance$yRelease <- round(release_distance$yRelease,2)
release_distance$Extension <- 60.5 - release_distance$yRelease
release_distance_sorted <- release_distance[order(release_distance$yRelease),]

#For matching with Savant Data
pitches$home<-toupper(substr(pitches$gameday_link,23,25))


#Needs data from Baseball Savant - which hosts Extension data - to do this part.
park <- "BAL"
data.compare <- read.csv(paste(park,"_data.csv",sep=""))
data.compare$home <- as.character(data.compare$home_team)
data.merge <- merge(data.compare,pitches,by=c("sv_id","home"))
data.merge$method_diff <- data.merge$Extension-as.numeric(as.character(data.merge$release_extension))
plot(data.merge$Extension,as.numeric(as.character(data.merge$release_extension)),ylim=c(4,7),xlim=c(4,7),ylab="Statcast Extension (mlb.com)",xlab="Estimated Extension (this paper)",main=park,cex=.1)
text(data.merge$Extension,as.numeric(as.character(data.merge$release_extension)),ylim=c(4,7),xlim=c(4,7),labels = substr(data.merge$game_date,9,11),cex=.8)
abline(a=0,b=1)

#Replicate Analysis but use $start_speed.x, which is Baseball Savant Start Speed and Has Increased Precision 
a = data.merge$ax.y^2 + data.merge$ay.y^2 + data.merge$az.y^2
b = 2*(data.merge$ax.y*data.merge$vx0.y+data.merge$ay.y*data.merge$vy0.y+data.merge$az.y*data.merge$vz0.y)
c = data.merge$vx0.y^2 + data.merge$vy0.y^2 + data.merge$vz0.y^2 - (5280*as.numeric(as.character(data.merge$start_speed.x))/3600)^2
t = (-b-sqrt(b^2-4*a*c))/(2*a)
data.merge$yRelease2 = data.merge$y0+data.merge$vy0.y*t+.5*data.merge$ay.y*t^2
data.merge$Extension2 <- 60.5 - data.merge$yRelease2
plot(data.merge$Extension2,as.numeric(as.character(data.merge$release_extension)),ylim=c(4,7),xlim=c(4,7),ylab="Statcast Extension (mlb.com)",xlab="Estimated Extension (this paper)",main=park)
abline(a=0,b=1)
plot(data.merge$Extension,as.numeric(as.character(data.merge$release_extension)),ylim=c(4,7),xlim=c(4,7),ylab="Statcast Extension (mlb.com)",xlab="Estimated Extension (this paper)",main=park)


data.merge$release_extension <- as.numeric(as.character(data.merge$release_extension))
release_distance_both <- aggregate(cbind(Extension2,release_extension)~pitcher_name,data=data.merge,FUN=mean)
plot(release_distance_both$Extension2,release_distance_both$release_extension,ylim=c(4,8),xlim=c(4,8),ylab="Statcast Extension (mlb.com)",xlab="Estimated Extension (this paper)",pch=19,main=park)
text(release_distance_both$Extension2,release_distance_both$release_extension,labels = release_distance_both$pitcher_name,ylim=c(4,8),xlim=c(4,8),cex=.6)
#plot(data.merge$Extension,,ylim=c(3,7),xlim=c(3,7),col=(pitches$x0>0)+1)
abline(a=0,b=1)
