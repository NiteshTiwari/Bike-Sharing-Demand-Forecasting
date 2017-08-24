## Data Cleaning 
#install.packages(c("plyr","dplyr","reshape2","tidyr","stringr"))
require(plyr)
require(dplyr)
require(tidyr)
require(stringr)
require(reshape2)
require(lubridate)


# Create a data directory 
if (file.exists("~/Capitalbikeshare")){
  setwd("~/Capitalbikeshare")
} else {
  dir.create("~/Capitalbikeshare")
  setwd("~/Capitalbikeshare")
}


year=c("2011","2012","2013","2014","2015")
quarter=c("Q1","Q2","Q3","Q4")

#Download the file
for(i in 1:length(year)){
  for(j in 1:length(quarter)){
    #download.file(paste("https://s3.amazonaws.com/capitalbikeshare-data/",year[i],"-",quarter[j],"-cabi-trip-history-data.zip",sep=""),paste(year[i],quarter[j],".zip",sep=""))
    data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
    assign(paste(quarter[j],year[i],sep=""),data)
    }
}


#Used the last two quarter data
i=5
j=3
data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
colnames(data)[1]="Duration"
data$Duration=data$Duration/1000
colnames(data)[dim(data)[2]]="Member.Type"
data$Member.Type[data$Member.Type=="Member"]="Registered"
colnames(data)[c(4,5,6,7)]=c("Start.station.number","Start.station","End.station.number","End.station")
data=datacleaner2(data)
assign(paste(quarter[j],year[i],sep=""),data)

data=Q32015[,c(4,5)]
start=unique(data)
dim(data)
data=Q32015[,c(6,7)]
end=unique(data)
dim(data)

# Extract the station number into another columns
# for 2011 Q1- 2011 Q4
datacleaner1=function(data){
  data=data%>%separate(col="Start.station",into=c("Start.station","Start.station.number"),sep=-9)%>%separate(col="End.station",into=c("End.station","End.station.number"),sep=-9)
  data$Start.station.number=gsub(" ","",data$Start.station.number)
  data$End.station.number=gsub(" ","",data$End.station.number)
  data$Start.station.number=substr(data$Start.station.number,2,6)
  data$End.station.number=substr(data$End.station.number,2,6)
  return(data)
}

datacleaner2=function(data){
  data$Start.station[grepl("New Hampshire Ave & T St NW",data$Start.station)]="New Hampshire Ave & T St NW"
  data$End.station[grepl("New Hampshire Ave & T St NW",data$End.station)]="New Hampshire Ave & T St NW"
  data$Start.station[grepl("17th & K St NW",data$Start.station)]="17th & K St NW"
  data$End.station[grepl("17th & K St NW",data$End.station)]="17th & K St NW"
  return(data)
}

#fixdate function
fixdate=function(data){
  
  #Separate start.date into start date and time
  data=data%>% separate(col="Start.date",into=c("Start.date","Start.time"),sep=" ")
  #Separate start.date into end date and time
  data=data%>% separate(col="End.date",into=c("End.date","End.time"),sep=" ")
  
  #Start Date
  data=data%>%separate(col="Start.date",into=c("month","day","year"),sep="/")
  data$month[nchar(data$month)==1]=paste(0,data$month[nchar(data$month)==1],sep="")
  data$day[nchar(data$day)==1]=paste0(0,data$day[nchar(data$day)==1],sep="")
  data$Start.date=paste(data$month,data$day,data$year,sep=":");data$month=NULL;data$day=NULL;data$year=NULL
  
  #Start Time
  data=data%>%separate(col="Start.time",into=c("hour","minute"),sep=":")
  data$hour[nchar(data$hour)==1]=paste(0,data$hour[nchar(data$hour)==1],sep="")
  data$Start.time=paste(data$hour,data$minute,sep=":");data$hour=NULL;data$minute=NULL
  
  #Combine Start Date and Start Time
  data$Start.time=paste(data$Start.date,data$Start.time,sep=" ");data$Start.date=NULL
  
  #Convert the date into POIStct
  data$Start.time=as.POSIXct(strptime(data$Start.time,format="%m:%d:%Y %H:%M"))
  
  #End Date
  data=data%>%separate(col="End.date",into=c("month","day","year"),sep="/")
  data$month[nchar(data$month)==1]=paste(0,data$month[nchar(data$month)==1],sep="")
  data$day[nchar(data$day)==1]=paste0(0,data$day[nchar(data$day)==1],sep="")
  data$End.date=paste(data$month,data$day,data$year,sep=":");data$month=NULL;data$day=NULL;data$year=NULL
  
  #End Time
  data=data%>%separate(col="End.time",into=c("hour","minute"),sep=":")
  data$hour[nchar(data$hour)==1]=paste(0,data$hour[nchar(data$hour)==1],sep="")
  data$End.time=paste(data$hour,data$minute,sep=":");data$hour=NULL;data$minute=NULL
  
  #Combine End Date and End Time
  data$End.time=paste(data$End.date,data$End.time,sep=" ");data$End.date=NULL
  
  #Convert the date into POIStct
  data$End.time=as.POSIXct(strptime(data$End.time,format="%m:%d:%Y %H:%M"))
  
  return(data)
}

#fixdate2 is made only for 2014 quarter 3 and 4 for differring format. 
fixdate2=function(data){
  
  #Separate start.date into start date and time
  data=data%>% separate(col="Start.date",into=c("Start.date","Start.time"),sep=" ")
  #Separate start.date into end date and time
  data=data%>% separate(col="End.date",into=c("End.date","End.time"),sep=" ")
  
  #Start Date
  data=data%>%separate(col="Start.date",into=c("year","month","day"),sep="-")
  data$month[nchar(data$month)==1]=paste(0,data$month[nchar(data$month)==1],sep="")
  data$day[nchar(data$day)==1]=paste0(0,data$day[nchar(data$day)==1],sep="")
  data$Start.date=paste(data$month,data$day,data$year,sep=":");data$month=NULL;data$day=NULL;data$year=NULL
  
  #End Date
  data=data%>%separate(col="End.date",into=c("year","month","day"),sep="-")
  data$month[nchar(data$month)==1]=paste(0,data$month[nchar(data$month)==1],sep="")
  data$day[nchar(data$day)==1]=paste0(0,data$day[nchar(data$day)==1],sep="")
  data$End.date=paste(data$month,data$day,data$year,sep=":");data$month=NULL;data$day=NULL;data$year=NULL
  
  
  #Start Time
  data=data%>%separate(col="Start.time",into=c("hour","minute"),sep=":")
  data$hour[nchar(data$hour)==1]=paste(0,data$hour[nchar(data$hour)==1],sep="")
  data$Start.time=paste(data$hour,data$minute,sep=":");data$hour=NULL;data$minute=NULL
  
  #Combine Start Date and Start Time
  data$Start.time=paste(data$Start.date,data$Start.time,sep=" ");data$Start.date=NULL
  
  #Convert the date into POIStct
  data$Start.time=as.POSIXct(strptime(data$Start.time,format="%m:%d:%Y %H:%M"))
  
  
  
  #End Time
  data=data%>%separate(col="End.time",into=c("hour","minute"),sep=":")
  data$hour[nchar(data$hour)==1]=paste(0,data$hour[nchar(data$hour)==1],sep="")
  data$End.time=paste(data$hour,data$minute,sep=":");data$hour=NULL;data$minute=NULL
  
  #Combine End Date and End Time
  data$End.time=paste(data$End.date,data$End.time,sep=" ");data$End.date=NULL
  
  #Convert the date into POIStct
  data$End.time=as.POSIXct(strptime(data$End.time,format="%m:%d:%Y %H:%M"))
  
  return(data)
}

# Fix duration
fixduration=function(data){
  data=data%>%separate(col="Duration",into=c("hour","minute","second"),sep=" ")
  regexp <- "[[:digit:]]+"
  data$hour=as.numeric(str_extract(data$hour, regexp))
  data$minute=as.numeric(str_extract(data$minute, regexp))
  data$second=as.numeric(str_extract(data$second, regexp))
  data$Duration=data$hour*3600+data$minute*60+data$second #Duration in seconds
  data$hour=NULL; data$minute=NULL;data$second=NULL
  return(data)
}

# checkout hourly and checkin hourlytime function
# use starting hour; 23:58 will be classified as 23:00 
in_n_out=function(data){
  data$out_hour[minute(data$Start.time)==0]=data$Start.time[minute(data$Start.time)==0]
  data$out_hour[!minute(data$Start.time)==0]= data$Start.time[!minute(data$Start.time)==0]-minute(data$Start.time[!minute(data$Start.time)==0])*60
  data$out_hour=as.POSIXct(data$out_hour, origin = "1970-01-01")  
  
  data$in_hour[minute(data$End.time)==0]=data$End.time[minute(data$End.time)==0]
  data$in_hour[!minute(data$End.time)==0]= data$End.time[!minute(data$End.time)==0]-minute(data$End.time[!minute(data$End.time)==0])*60
  data$in_hour=as.POSIXct(data$in_hour, origin = "1970-01-01")  
  return(data)
}

# second_data function will process the raw data and calculate the number of bikes checked out and checked in from each station
second_data=function(data){
  data=data%>%select(Start.station.number,End.station.number,out_hour,in_hour)
  data1=data%>%group_by(out_hour,Start.station.number)%>%summarise(Checked_Out=n())%>% rename (hour=out_hour,Station.number=Start.station.number)
  data2=data%>%group_by(in_hour,End.station.number)%>%summarise(Checked_In=n()) %>% rename(hour=in_hour,Station.number=End.station.number)
  
  data=merge(data1,data2,by.y=c("hour","Station.number"))
  data$Net_Out=data$Checked_Out-data$Checked_In
  data=data%>%group_by(Station.number)%>%mutate(cum_Net_Out = cumsum(Net_Out))
  data$date=data$hour
  data$year=year(data$hour)
  data$month=month(data$hour)
  data$day=wday(data$hour)
  data$hour=hour(data$hour)
  data$workday[data$day %in% c(1,2,3,4,5)]=1
  data$workday[data$day %in% c(6,7)]=0
  data=data%>%select(date,year,month,day,hour,workday,Station.number,Checked_Out,Checked_In,Net_Out,cum_Net_Out)
}


demand_station=data.frame()

for(i in 2:length(year)){
  for(j in 1:length(quarter)){
    if(i ==1){
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[dim(data)[2]]="Member.Type"
      data$Member.Type[data$Member.Type=="Subscriber"]="Registered"
      colnames(data)[c(4,5)]=c("Start.station","End.station")
      data=datacleaner1(data)
      data=datacleaner2(data)
  
      data=fixdate(data)
      data=fixduration(data)
      data=in_n_out(data)
      data=second_data(data)
    } else if (i ==2){
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[dim(data)[2]]="Member.Type"
      data$Member.Type[data$Member.Type=="Subscriber"]="Registered"
      colnames(data)[c(4,5)]=c("Start.station","End.station")
      data=datacleaner2(data)
      
      data=merge(data,start,by.y="Start.station")
      data=merge(data,end,by.y="End.station")
      
      data=fixdate(data)
      data=fixduration(data)
      data=in_n_out(data)
      data=second_data(data)
      
    } else if(i ==3 & j==2){
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[dim(data)[2]]="Member.Type"
      data$Member.Type[data$Member.Type=="Subscriber"]="Registered"
      data=data%>% rename(Start.date=Start.time)
      colnames(data)[c(4,5)]=c("Start.station","End.station")
      data=datacleaner2(data)
      
      data=merge(data,start,by.y="Start.station")
      data=merge(data,end,by.y="End.station")
      
      
      data=fixdate(data)
      data=fixduration(data)
      data=in_n_out(data)
      data=second_data(data)
    }  else if(i ==3 & j%in% c(1,3)){
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[dim(data)[2]]="Member.Type"
      data$Member.Type[data$Member.Type=="Subscriber"]="Registered"
      colnames(data)[c(4,5)]=c("Start.station","End.station")
      data=datacleaner2(data)
      
      data=merge(data,start,by.y="Start.station")
      data=merge(data,end,by.y="End.station")
      
     
      data=fixdate(data)
      data=fixduration(data)
      data=in_n_out(data)
      data=second_data(data)
    } else if (i ==3 & j%in% c(4)){
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[dim(data)[2]]="Member.Type"
      data$Member.Type[data$Member.Type=="Subscriber"]="Registered"
      colnames(data)[c(3,5)]=c("Start.station","End.station")
      data=datacleaner2(data)
      
      data=merge(data,start,by.y="Start.station")
      data=merge(data,end,by.y="End.station")
      
      data=fixdate(data)
      data=fixduration(data)
      data=in_n_out(data)
      data=second_data(data)
    } else if(i ==4 & j %in% c(1,2,3)){
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[dim(data)[2]]="Member.Type"
      data$Member.Type[data$Member.Type=="Subscriber"]="Registered"
      colnames(data)[c(3,5)]=c("Start.station","End.station")
      data=datacleaner2(data)
      
      data=merge(data,start,by.y="Start.station")
      data=merge(data,end,by.y="End.station")
     
      
      data=fixdate(data)
      data=fixduration(data)
      data=in_n_out(data)
      data=second_data(data)
      
    } else if(j ==4 & i==4){
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[dim(data)[2]]="Member.Type"
      colnames(data)[1]="Duration"
      data$Member.Type[data$Member.Type=="Subscriber"]="Registered"
      colnames(data)[c(3,5)]=c("Start.station","End.station")
      data=datacleaner2(data)
      
      data=merge(data,start,by.y="Start.station")
      data=merge(data,end,by.y="End.station")
      
      data=fixdate2(data)
      data=fixduration(data)
      data=in_n_out(data)
      data=second_data(data)
      
    } else if (i ==5 & j %in% c(1,2)) {
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[1]="Duration"
      data$Duration=data$Duration/1000
      colnames(data)[dim(data)[2]]="Member.Type"
      data$Member.Type[data$Member.Type=="Member"]="Registered"
      colnames(data)[c(3,5)]=c("Start.station","End.station")
      data=datacleaner2(data)
      data=merge(data,start,by.y="Start.station")
      data=merge(data,end,by.y="End.station")
      assign(paste(quarter[j],year[i],sep=""),data)
      data=fixdate(data)
      data=in_n_out(data)
      data=second_data(data)
    } else if (i ==5 & j %in% c(3,4)) {
      data=read.csv(unzip(paste("./",year[i],quarter[j],".zip",sep="")),header=TRUE,stringsAsFactors = FALSE)
      colnames(data)[1]="Duration"
      data$Duration=data$Duration/1000
      colnames(data)[dim(data)[2]]="Member.Type"
      data$Member.Type[data$Member.Type=="Member"]="Registered"
      colnames(data)[c(4,5,6,7)]=c("Start.station.number","Start.station","End.station.number","End.station")
      data=datacleaner2(data)
      
      assign(paste(quarter[j],year[i],sep=""),data)
      data=fixdate(data)
      data=in_n_out(data)
      data=second_data(data)
    } 
    
    demand_station=rbind(demand_station,data)
    #demand=rbind(demand,data)
    print(i)
    print(j)
  }
}



# Extract information from XML file
require(XML)

xmlToList(books)

books <- "bikeStations.xml"
library(plyr)
ldply(xmlToList(books), function(x) { data.frame(t(unlist(a[4]))) } )
a=xmlToList(books)

loglat=data.frame()
for (i in 2:377){
        b=data.frame(t(unlist(a[i])),stringsAsFactors=FALSE) 
        loglat=rbind(loglat,b)
        print(i)
}


loglat=loglat[,c(2,3,5:13)]
loglat=loglat[,c(1:4,10:11)]
names(loglat)=c("Station.name","Station.number","Lat","Long","bikes","docks")
loglat=within(loglat,{bikes=as.numeric(bikes) ;Lat=as.numeric(Lat) ;Long=as.numeric(Long); docks=as.numeric(docks);total=bikes+docks})

loglat=read.csv("loglat.csv",stringsAsFactors = FALSE)
loglat1=loglat[,1:9]
loglat2=na.omit(loglat[,9:dim(loglat)[2]])
loglat=merge(loglat1,loglat2,by="ZipCode")  
loglat$X=NULL

# Demand data
demand_station$total=demand_station$Checked_In+demand_station$Checked_Out
lesstpopular=demand_station[demand_station$Station.number==popular$Station.number[which.min(popular$total)],]
lesstpopular$cum_Net_Out=cumsum(lesstpopular$Net_Out)
popular=demand_station%>%group_by(Station.number)%>%summarise(total=sum(total))%>%arrange(desc(total))
station=popular$Station.number


# A regression model on the demand for the most popular station
for( i in 1:length(station)){
mostpopular=demand_station[demand_station$Station.number==station[i],]
mostpopular$cum_Net_Out=cumsum(mostpopular$Net_Out)
qplot(y=cum_Net_Out,x=date,data=mostpopular,geom="line",main=paste("Station Number: ",station[i],"Total Trip:",popular$total[popular$Station.number==station[i]]))+geom_smooth(method="lm",se=TRUE)
}

demand1=demand_station%>%group_by(Station.number)%>%mutate(totaltrip=sum(total))
demand1=merge(demand1,loglat,by="Station.number")

log=within(log,{bikes=NULL;docks=NULL;Longitude.x=NULL;Latitude.x=NULL})
log1=log%>%group_by(Station.number)%>%filter(row_number()==1)
