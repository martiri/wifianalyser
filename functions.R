
#Before starting this code you must make sure you have extracted the capture correctly with ####
# the following columns and with exactly the same name:
# $ No.                  -------------> The number of packet
# $ Time                 -------------> The moment in time the packet was captured
# $ Transmitter.address  -------------> The transmitter address (MAC)
# $ Source               -------------> The source address (MAC)
# $ Destination          -------------> 
# $ BSS.Id               -------------> The BSS ID of each packet. i.e. the AP addres (MAC)
# $ SSID                 -------------> The name with we can recognize the network
# $ Channel              -------------> The channel the packet is travelling trhough
# $ Signal.strength..dBm.-------------> The RSSI in (dBm)
# $ Type.Subtype         -------------> The subtype of packet (Beacon frame, Acknowledgement, etc.)
# $ Subtype              -------------> The subtype code recognized by Wireshark
# $ Length               -------------> The length of each packet
# $ Type                 -------------> The type of packet (Management, Data or Control frame)
# $ DS.status            -------------> This is the status it tells you if the packet is being send to an ap or from an ap
# $ Current.Channel      -------------> The channel the packet is suposed to be travelling through
# $ Receiver.address     -------------> 
# $ Duration             -------------> The duration of the packet in be transmitted (micro seconds)
# $ Retry                -------------> This tels you if a packet has been retransmitted or not


#install.packages("plyr")#######################################
#we call the library plyr (installed previously)
library(plyr)



#first##################################################################################################

#In this function the variable input is the name of file you want to import
#Make sure the name is between brackets
export_aps_data <- function(input){
  
  #creates a data frame with the input (in this case the capture exported in csv format from wireshark)
  capture <- read.table(input, header=TRUE,sep=",", stringsAsFactors=FALSE)
  
  #creates a data frame containing only the beacons packets 
  beacons <- subset(capture, Type.Subtype == "Beacon frame")
  
  #creates a data frame with all the aps detected in the capture by checking the beacon packets which are only
  # send by Access Points 
  aps <- ddply(beacons, .(BSS.Id, SSID, Current.Channel), summarise, RSSI=mean(Signal.strength..dBm.))
  aps<- aps[order(aps$SSID, decreasing=TRUE),]
  
  #returns a list of the capture, beacon and aps data frames
  return(list(capture= capture, beacon =beacons, aps=aps))

  }

#Rssi#################################

#This function will be in charge of ploting all the Aps showing their mean RSSI in a histogram.
#The ploting wil be saved in a png file in the same folder the program is executing.
rssi <- function(){
  
  #You create a png file
  png(file="RSSI.png")
  
  #You plot the histogram inside the png file
  barplot( aps$RSSI, names.arg = aps$BSS.Id, xlab="AP", ylab="RSSI",  border="red", las=2)
  
  #You close the png file
  dev.off()

  }

#second#################################################################################################

#This function is on charge of getting the information from each channel you have been sniffing

channel_data <- function(capture){
  
  #Creates a data frame with 2 columns ( Channel and Time it has been scaning)
  Channel.Data <- data.frame(Channel=integer(),time.scanning=numeric(),stringsAsFactors=FALSE)
  
  
  #initiate provisional variables to be able to calculate later the amount of time you 
  # have been sniffing the channel
  firstt=0.0000
  lastt=0.0000
  time=0.0000
  channel=0
  
  #A loop that goes through all the capture table
  for (i in 1:nrow(capture)){
    
    #This is for the first time you enter to the function
    #It is to get the first channel you started sniffing
    if(channel==0 ){
      
      channel=capture$Channel[i]
      
      }
    
    #A condition to check if the capture has changed of channel and if the channel is inside the
    # Channel.Data table to rewrite the inside
    if (channel != capture$Channel[i] && channel %in% Channel.Data$Channel == TRUE){
      
      #if it is a new channel you get the time of the previous packet to get when it finished sniffing the
      # previous channel
      lastt=capture$Time[i-1]
      
      #You make an operation to calcute the total time you have been sniffing the channel
      time=subset(Channel.Data, Channel == channel)$time.scanning + (lastt-firstt)
      
      #NO FUNCIONA SI NO CANVIA DE CANAL EN NINGUN MOMENT (CREC)
      #You get the number of row the current channel is in the Channel.Data table
      n=which(Channel.Data$Channel==channel)
      
      #You create  vector with the time it started and finshed to asniff a channel
      n1<-c(firstt,lastt)
      
      #You save the vector in the intervals column which will store all the pairs of times
      #This will be useful in other functions
      Channel.Data$intervals[[n]]<- c(Channel.Data$intervals[[n]], n1)
      
      
      
      #You store the moment on time it started to sniff the new channel
      firstt=capture$Time[i]
      
      #You store the time you calculated previously (This variable variates if it founds there has been another
      # period of time analysing the same channel )
      Channel.Data$time.scanning[Channel.Data$Channel == channel ] <- time
      
      #Stores the number of the current channel in a provisional variable
      channel = capture$Channel[i]
      
      
      #If not, a condition it checks if the current channel in the capture does not exist in the Channel.Data table
      } else if( channel != capture$Channel[i] && channel %in% Channel.Data$Channel == FALSE){
      
      #Stores the moment of time it stopped analysing the previous channel
      lastt=capture$Time[i-1]
      
      #Gets the total time the previous channel has been sniffed
      time=lastt-firstt
      
      n1<-c(firstt,lastt)
      
      #Adds the new channel in the Channel.Data table
      Channel.Data <- rbind(Channel.Data, data.frame(Channel = channel, time.scanning=time, intervals=NA))
      
      #Adds the new interval of time in the intervals column
      n=which(Channel.Data$Channel==channel)
      Channel.Data$intervals[n]<- list(n1)
      
      #Stores the new channel number and the first moment in time of the new channel
      channel = capture$Channel[i]
      firstt=capture$Time[i]
      
      }
    
    #Condition to check if you are in the last packet of the capture
    #This is to get the last interval of time of the last channel sniffed of the capture
    if (i==nrow(capture)){
      
      #Condition to check if the current channel is in the Channel.Data table
      if (channel %in% Channel.Data$Channel == TRUE){
        
        
        lastt=capture$Time[i]
        
        time=subset(Channel.Data, Channel == channel)$time.scanning + (lastt-firstt)
        
        
        n=which(Channel.Data$Channel==channel)
        
        n1<-c(firstt,lastt)
        
        Channel.Data$intervals[[n]]<- c(Channel.Data$intervals[[n]], n1)
        
        
        Channel.Data$time.scanning[Channel.Data$Channel == channel ] <- time
        
        
        #Condition to check if the current channel is not in the Channel.Data table
        }else if(channel %in% Channel.Data$Channel == FALSE){
        
        lastt=capture$Time[i]
        
        time=lastt-firstt
        
        n1<-c(firstt,lastt)
        
        Channel.Data <- rbind(Channel.Data, data.frame(Channel = channel, time.scanning=time, intervals=NA))
        
        n=which(Channel.Data$Channel==channel)
        
        Channel.Data$intervals[n]<- list(n1)
        
        }
      
      }
    
    }
  
  return(Channel.Data)
  
  }

#third#########################################################################################################

#This function will be in charge of getting additional information such as the retransmitions or the rates
# of the different types of packets and throughput.
extended_aps_table <- function(){
  
  #Initializing  variables
  totaltime.mf=0.0000
  totaltime.df=0.0000
  totaltime.cf=0.0000
  totalpackets=0.0000
  retransmitted.mf=0.0000
  retransmitted.df=0.0000
  retransmitted=0.0000
  reliability=0.0000
  bytes= 0.0000
  timechannel=0.0000
  
  #Adding 3 new columns to the aps table
  aps["Management.Rate"] <- 0.0000
  aps["Data.Rate"] <- 0.0000
  aps["Control.Rate"] <- 0.0000
  aps["Throughput.Mbps"] <- 0.0000
  
  sum=0
  
  for ( i in 1:nrow(aps)){
    
    #Creating subset of tables for each type of packets
    provisional.mf <-subset(capture, BSS.Id == aps$BSS.Id[i] & Type=="Management frame")
    provisional.df <-subset(capture, BSS.Id == aps$BSS.Id[i] & Type=="Data frame" & !grepl("AD-HOC",capture$DS.status ))
    provisional.cf <-subset(capture, (Transmitter.address == aps$BSS.Id[i] | Receiver.address == aps$BSS.Id[i])
                            & Type=="Control frame")
    

    w=which(Channel.Data$Channel==aps$Current.Channel[i])
    timechannel = Channel.Data$time.scanning[w]
    
    
    #Gets the total packets each AP has been transmitting, the number of packets that have been retransmitted 
    # and calculates the retransmision rate (reliability)
    totalpackets=nrow(provisional.mf)+nrow(provisional.df)
    retransmitted= nrow(subset(provisional.df, grepl("Frame is being retransmitted",provisional.df$Retry)))
    reliability=retransmitted/totalpackets
    
    #Vectors with the different channels the AP has been sending
    p.mf <- unique(provisional.mf$Channel, incomparables = FALSE)
    p.df<- unique(provisional.df$Channel, incomparables = FALSE)
    p.cf <- unique(provisional.cf$Channel, incomparables = FALSE)
    
    #Loops that go through all the channels
    for ( j in 1:length(p.mf)){
      
      totaltime.mf=totaltime.mf+Channel.Data$time.scanning[Channel.Data$Channel == p.mf[j]]
    
      }
    for ( j in 1:length(p.df)){
      
      totaltime.df=totaltime.df+Channel.Data$time.scanning[Channel.Data$Channel == p.df[j]]
    
      }
    for ( j in 1:length(p.cf)){
      
      totaltime.cf=totaltime.cf+Channel.Data$time.scanning[Channel.Data$Channel == p.cf[j]]
      
      }

    #Calculates the management rate and appends it into the corresponding AP
    aps$Management.Rate[i] = nrow(provisional.mf)/totaltime.mf
    
    #Condition to check if there is any Data packets
    if(nrow(provisional.df)!=0){
      
      aps$Data.Rate[i]=nrow(provisional.df)/totaltime.df
      bytes = sum(provisional.df$Length)
    
      } else{
      
        #In case there is not any data packets the data rate will be 0
        aps$Data.Rate[i]=0
    
      }
    
    if(nrow(provisional.cf)!=0){
      
      aps$Control.Rate[i]=nrow(provisional.cf)/totaltime.cf
    
      } else{
      
      aps$Control.Rate[i]=0
    
    }
    
    #Finally it appends the reliability puts the totaltime to 0 again
    aps$Retransmision[i]=1-reliability
    
    if(timechannel==0){
      
      aps$Throughput.Mbps[i]=0
      
    }else{
      
      throughput=(bytes*8)/timechannel
      aps$Throughput.Mbps[i]=throughput/1e6
      
    }
    
    
    bytes=0.0000
    totaltime.mf=0.0000
    totaltime.df=0.0000
    totaltime.cf=0.0000
  
    }
  
  return(aps)

  }

#forth########################################################################################################

#comprobar all.sta. Sembla que no es fagi el descarte
#This function will create a table with all the STA detected in the capture plus additional information
sta_table <- function(){
  
  #creates a data frame with all the MACs detected
  provisional <- ddply(capture, .(Transmitter.address,Receiver.address), summarise, RSSI=mean(Signal.strength..dBm.))
  
  #Converts the vector into a data frame and, at the same time combines the 2 columns in one
  all.sta<-as.vector(as.matrix(provisional[,c("Transmitter.address","Receiver.address")]))
  
  #Discards the duplicated MACs and the broadcast directions
  all.sta<- unique(all.sta,incomparables = FALSE)
  all.sta <- subset(all.sta, all.sta!="" & all.sta!="ff:ff:ff:ff:ff:ff")
  
  #Vector where it discards all the APs and stores the rest
  all.sta <- all.sta[ which( !(all.sta %in% aps$BSS.Id) )]
  
  #Converts the matrix into a frame called sta and put a name to the column
  all.sta1<-matrix(all.sta,nrow = length(all.sta),ncol = 1)
  all.sta1<- as.data.frame(all.sta1)
  all.sta1 <- data.frame(lapply(all.sta1, as.character), stringsAsFactors=FALSE)
  colnames(all.sta1)=c("Transmitter.address")
  stas <- all.sta1
  
  #Creates 3 more columns with value 0
  stas$Management.Rate <-0.0000
  stas$Data.Rate<-0.0000
  stas$Control.Rate <- 0.0000
  
  #Creates 3 provisional variables to get the time it has been receiving or transmitting each sta
  totaltime.mf=0.0000
  totaltime.df=0.0000
  totaltime.cf=0.0000
  
  #loop to get through all the STA in the stas table
  for ( i in 1:nrow(stas)){
    
    #Creates 3 provisional data frames storing the 3 types of packets of each STA
    provisional.mf <-subset(capture, (Transmitter.address ==stas$Transmitter.address[i] | Receiver.address==stas$Transmitter.address[i])
                            & Type=="Management frame")
    provisional.df <-subset(capture, (Transmitter.address ==stas$Transmitter.address[i] | Receiver.address==stas$Transmitter.address[i])
                            & Type=="Data frame" & (grepl("From DS: 0",capture$DS.status )|grepl("To DS: 0",capture$DS.status)))
    provisional.cf <-subset(capture, (Transmitter.address ==stas$Transmitter.address[i] | Receiver.address==stas$Transmitter.address[i])
                            & Type=="Control frame")
    
    #Creates 3 vectors with the channels that have each of the 3 types of packets
    p.mf <- unique(provisional.mf$Channel, incomparables = FALSE)
    p.df <- unique(provisional.df$Channel, incomparables = FALSE)
    p.cf <- unique(provisional.cf$Channel, incomparables = FALSE)
    
    for ( j in 1:length(p.mf)){
      
      #Total time th STA has been transmitting Management frames
      totaltime.mf=totaltime.mf+Channel.Data$time.scanning[Channel.Data$Channel == p.mf[j]]
      
      }
    for ( j in 1:length(p.df)){
      
      #Total time th STA has been transmitting Data frames
      totaltime.df=totaltime.df+Channel.Data$time.scanning[Channel.Data$Channel == p.df[j]]
    
      }
    for ( j in 1:length(p.cf)){
      
      #Total time th STA has been transmitting Control frames
      totaltime.cf=totaltime.cf+Channel.Data$time.scanning[Channel.Data$Channel == p.cf[j]]
    
      }
    
    #Condition to check if there are Management frames
    if(nrow(provisional.mf)!=0){
      
      #Get the management rate by dividing the number of rows (or management packets) between the total time
      # stored before
      stas$Management.Rate[i] = nrow(provisional.mf)/totaltime.mf
      
      } else{
      
      #In case there are no Management Packets the rate will be 0
      stas$Management.Rate[i]= 0
    
      }
    
    
    if(nrow(provisional.df)!=0){
      
      stas$Data.Rate[i]= nrow(provisional.df)/totaltime.df
      
      } else{
      
      stas$Data.Rate[i]=0.0000
    
      }
    
    if(nrow(provisional.cf)!=0){
      
      stas$Control.Rate[i]= nrow(provisional.cf)/totaltime.cf
    
      } else{
      
      stas$Control.Rate[i]=0.0000
      
      }
    
    #Reinicialize the provisional variables
    totaltime.mf=0.0000
    totaltime.df=0.0000
    totaltime.cf=0.0000
    
    }
  
  return(stas)

  }

#fifth########################################################################################################

#This function creates the histogram of all the posible APs
ap_histograms <- function(y){
  
  #Initialize a list and int variables
  provisional.list <- list()
  j = 0
  
  #Deletes the folder Histograms and creates a new one
  unlink("Histograms", recursive=TRUE)
  dir.create(file.path("Histograms"))
  
  #A loop to check all the APs from the aps table
  for (j in 1:nrow(aps)){
    
    #Creates a subset from the capture table with all the packets related with an AP
    approv <- subset(capture, (Transmitter.address==aps$BSS.Id[j]| Receiver.address==aps$BSS.Id[j])& Channel==aps$Current.Channel[j])
    
    if (nrow(approv)==0){
      
      nodata <- data.frame(Time= integer(0), x= numeric(0))
      provisional.list[[paste0("", aps$BSS.Id[j])]] <- nodata
      
    } else {
      
      #Transforms the duration of the packets into seconds and orders the table by time
      approv1 <- aggregate(approv$Duration/1e6, by=list(Time=approv$Time%/%1), FUN=sum)
      
      #Gets the number n of row where the channel the AP is using is in the Channel.Data table
      n=which(Channel.Data$Channel==aps$Current.Channel[j])
      
      #String with the name of the path we want to send the histograms
      folder<- paste("Histograms", aps$Current.Channel[j], sep="/")
      
      #Condition to check if the folder path exists
      if (!dir.exists(folder)){
        
        #If it does not exist you create a folder "Histograms"
        dir.create(file.path(folder))
        
        }
      
      #z to get the number of times the capture has been sniffing the channel
      z=length(Channel.Data$intervals[[n]])
      
      
      total=0
      
      #A loop to that goes from 1 to z jumping 2
      for(i in seq(1,z,2)){
        
        #Start to get the moment it started sniffing the channel
        start=Channel.Data$intervals[[n]][i]%/%1
        
        #Final to get the moment it stopped sniffing the channel
        final=(Channel.Data$intervals[[n]][i+1]%/%1)+1
        
        #A subset of the approv1 table where the time is bigger than the start and lower than the final
        #i.e. a subset of approv1 where the capture was sniffing the channel in an interval
        v <- subset(approv1, Time >= start & Time <= final)
        
        #This is to keep suming the time if there is been diferent intervals analysing the same channel
        v$Time= v$Time - start + total
        
        
        
        if (total==0){
          
          real<- v
          
          #Add to the provisional list a table with the instant of time and the % of 
          # time in the second it has been sending packets of each ap
          provisional.list[[paste0("", aps$BSS.Id[j])]]<- v
          
          }else {
          
          #If total!=0 this means the ap table allready exists
          real<- rbind(real, v)
          
          #Appends the extra information from the current AP
          provisional.list[[paste0("", aps$BSS.Id[j])]]<- rbind(aps$BSS.Id[j], real)
          
          }
        
        #Calculates the total time it is being sniffing a channel
        total=final-start+total
        
        #It creates a row from the instants of times there is not any activity from part of the current AP
        real2 <-merge(expand.grid(Time=0:(Channel.Data$time.scanning[[n]]+(length(Channel.Data[[n,3]]))/2)),real, all=TRUE)
  
        #Changes the NA values for 0. This happenes because you added new rows as you can see in the previous line
        real2$x[is.na(real2$x)] <- 0
        
        #Adds this table into the provisional list
        provisional.list[[paste0("", aps$BSS.Id[j])]]<-real2
        
        #Creates a string with the MAC addres of each AP deleting the ":" character
        file.name<-paste0("", aps$BSS.Id[j])
        file.name <- gsub(":", " ", file.name)
        
        #Creates a png file with the name file.name and stores the histogram made from the tables stored in the 
        # provisional list
        png(sprintf(paste("Histograms", "%d/%s.png", sep="/"), aps$Current.Channel[j],file.name))
        if (plothist==TRUE){
          
          barplot(real2$x, real2$Time, xlab="Time (s)", ylab="time used in each second (%)", ylim=c(0,yscale),width=1, space = 0.2, col = "black")
        
          }else{
          
            plot(real2$Time, real2$x, type="l", xlab="Time (s)", ylab="time used in each second (%)",ylim=c(0,yscale), lwd=2, col="red")
          
          }
        
        dev.off()
        
        }
   
       }
    
    }
  
  #In this loop you will take into consideration the packets that were not suposed to be in the channel but are
  # because of the overlapping
  for(i in 1:14){
    
    #This packets will be called "remains i" where i is the channel they belong to
    name <- paste("remains", i)

    #A subset with all the packets that do not correspond to the current channel due to overlapping
    # i.e. packets from APs that should not transmitt in this channel
    approv <- subset(capture, (!Transmitter.address %in% aps$BSS.Id & !Receiver.address%in%aps$BSS.Id & Channel==i)|(Channel==i & Current.Channel!=i & !is.na(Current.Channel)))
    
    if (nrow(approv)==0){
      
      nodata <- data.frame(Time= integer(0), x= numeric(0))
      provisional.list[[paste0("", name)]] <- nodata
      
    }else {
      
      approv1 <- aggregate(approv$Duration/1e6, by=list(Time=approv$Time%/%1), FUN=sum)
      
      
      w<- paste("Histograms", i, sep="/")
      
      if (!dir.exists(w)){
        
        dir.create(file.path(w))
        
        }
      
      n=which(Channel.Data$Channel==i)
      z=length(Channel.Data$intervals[[n]])
      
      total=0
      
      for(j in seq(1,z,2)){
        
        start=Channel.Data$intervals[[n]][j]%/%1
        final=(Channel.Data$intervals[[n]][j+1]%/%1)+1
        
        v <- subset(approv1, Time >=start & Time<=final)
        
        v$Time= v$Time - start + total
        
        if (total==0){
          
          real<- v
          provisional.list[[paste0("", name)]]<-v
          
        
          }else {
          
          real<- rbind(real, v)
          provisional.list[[paste0("", name)]]<-rbind(name, real)
        
          }
        
        
        total=final-start+total
  
        real2 <-merge(expand.grid(Time=0:(Channel.Data$time.scanning[[n]]+(length(Channel.Data[[n,3]]))/2)),real, all=TRUE)
        real2$x[is.na(real2$x)] <- 0
        
        provisional.list[[paste0("", name)]]<-real2
        
        png(sprintf(paste("Histograms", "%d/%s.png", sep="/"), i,name))
        
        if (plothist==TRUE){
        
          barplot(real2$x, real2$Time, xlab="Time (s)", ylab="time used in each second (%)",ylim=c(0,yscale),width=1, space = 0.2, col = "black")
        
          }else{
          
            plot(real2$Time, real2$x, type="l", xlab="Time (s)", ylab="time used in each second (%)",ylim=c(0,yscale), lwd=2, col="red")
          
            }
        dev.off()
        
        }
    
      }
    
    }
  
  return(provisional.list)
  
  }

#sixth########################################################################################################

#Gets the total correlation between each pair of APs in the same channel
total_correlation <- function(i){
  
  #Subset with all the APs that work in the same channel
  provisional <- subset(aps,aps$Current.Channel==i )
  
  #Creates a data frame with only the names of the APs + the "remains i" in each row
  aps.name <- data.frame(provisional$BSS.Id, stringsAsFactors=FALSE)
  aps.name <- rbind(aps.name, paste0("remains ", i))
  
  
  #A loop that goes through all the aps in the channel
  for(j in 1:nrow(aps.name)){
    
    #Creates a column with each name of AP
    aps.name[paste0(aps.name[j,1],"")]<-NA
  
    }
  
  for (j in 1:nrow(aps.name)){
    
    for (l in 1:nrow(aps.name)){
      
      if(j!=l){
        
        nrow1 <- sapply(apinfo[paste(aps.name[j,1],"",sep="")], nrow)[[1]]
        nrow2 <- sapply(apinfo[paste(aps.name[l,1],"",sep="")], nrow)[[1]]
        
        if (nrow1!=0 && nrow2!=0){

          #Calculates the correlation between each pair of APs
          correlation <- cor(as.data.frame(apinfo[paste(aps.name[j,1],"",sep="")])[2],
                             as.data.frame(apinfo[paste(aps.name[l,1],"",sep="")])[2])
          
          #Adds the obtained correlation to the aps.name table
          aps.name[j,l+1]<-correlation[1,1]
          
        } else{
          aps.name[j,l+1]<-0.0000
          
      
            }
        
        }
    
      }
  
    }
  
  return(aps.name)

  }

#seventh###############################################################################################

#Gets the correlation each x seconds between each pair of APs in the same channel
correlation_xseg <- function(i,x){
    
  if (i %in% Channel.Data$Channel){
    
      provisional <- subset(aps,aps$Current.Channel==i )
      provisional <- rbind(provisional, paste0("remains ", i))
      
      #Stores the the total time it has been sniffing the channel into the time variable
      time <- (Channel.Data$time.scanning[Channel.Data$Channel==i]%/%1)+1
      
      #Stores the time divided by x
      tentime <- (time%/%x)+1
      
      #Creates a table with the times jumping x units every time as rows
      provisional2 <- expand.grid(Time=seq(x,tentime*x,x))
      k <- 1
      
      apsname <- provisional$BSS.Id
      
      #Condition to check if there is 2 or more APs in the channel
      if(length(apsname) >= 2){
        
        for(j in 1:(length(apsname)-1)){
          
          if(j!=length(apsname)){
            
            h <- j+1
          
            }
          
          for (l in h:length(apsname)){
            
            #k to know in what x seconds we are calculating the correlation 
            k<- k+1
            
            #Creates a column with the name of the pair of APs
            provisional2[paste(apsname[j],apsname[l],sep = " // ")]<-NA
  
            for (r in 1:nrow(provisional2)){
              
              if(r!=nrow(provisional2)){
                
                #gets the x seconds info of the first ap
                interval1 <- as.data.frame(apinfo[paste(apsname[j],"",sep="")])[(provisional2[r,1]-(x-1)):provisional2[r,1],2]
                
                #gets the x seconds info of the second ap
                interval2 <- as.data.frame(apinfo[paste(apsname[l],"",sep="")])[(provisional2[r,1]-(x-1)):provisional2[r,1],2]
                if(any(is.na(interval1))==FALSE){
                  sdinterval1<- sd(interval1)
                }else{
                  sdinterval1 <- 0
                }
                if(any(is.na(interval2))==FALSE){
                  sdinterval2<- sd(interval2)
                }else{
                  sdinterval2 <- 0
                }
                
                if (sdinterval1!=0 && sdinterval2!=0){
                  
                  correlation <- cor(interval1,interval2)
  
                  provisional2[r,k]<- correlation
                
                } else {
                  
                  provisional2[r,k]<- 0.0000
                  
                  }
            
                }else if (r == nrow(provisional2)){ # last x seconds might not be x seconds
                
                #Gets the last seconds sniffed of the first AP
                interval1 <- as.data.frame(apinfo[paste(apsname[j],"",sep="")])[(provisional2[r,1]-(x-1)):time,2]
                
                #Gets the last seconds sniffed of the second AP
                interval2 <- as.data.frame(apinfo[paste(apsname[l],"",sep="")])[(provisional2[r,1]-(x-1)):time,2]
                
                if(any(is.na(interval1))==FALSE && length(interval1)>1){
                  sdinterval1<- sd(interval1)
                }else{
                  sdinterval1 <- 0
                }
                if(any(is.na(interval2))==FALSE && length(interval2)>1){
                  sdinterval2<- sd(interval2)
                }else{
                  sdinterval2 <- 0
                }
                
                if (sdinterval1!=0 && sdinterval2!=0){
                  
                  correlation <- cor(interval1,interval2)
                  provisional2[r,k]<- correlation
                
                }else {
                  
                  provisional2[r,k]<- 0.0000
                  
                  }
              
                }
            
              }
          
            }
          
          }
      
        }
    
    return(provisional2)

      }
    }


#Eighth###################################################
Relation <- function(i, limitcor){

# Checks What APs are related between them
if(exists(paste0("segCor",i))){
  
    cor<- get(paste0("segCor",i))
    
    #Get the mean of the correlation
    cor<-colMeans(x=cor, na.rm = TRUE)
    cor <- t(cor)
    
    #Removes the Time column
    cor<-subset(cor, select= -c(Time))
    
    
    if( ncol(cor)>0){
        
      #Adds 2 cells in each column with the AP name of each column title  
      cor <- rbind(cor, lapply(strsplit(colnames(cor), " // "), "[",1))
      cor <- rbind(cor, lapply(strsplit(colnames(cor), " // "), "[",2))
      
      #Creates a table with the APs name of each channel and a Relations column of type list()
      x <- data.frame(AP= character(nrow(subset(aps, Current.Channel == i))))
      y <- subset(aps, Current.Channel == i)
      x$AP <- y$BSS.Id
      x$Relations <- list(0)
      
      #Discards all the relations bigger than the limitcor global variable
      cor<-cor[,cor[1,]< limitcor, drop=FALSE]
      
      m<- list()
      
      if(ncol(cor)>0){
        
        for (j in 1:ncol(cor)){
          
          if (!grepl("remains", cor[3,j] ) ){
            
            #Checks the position of the Aps in the x Table
            n= which(x$AP == cor[2,j] | x$AP == cor[3,j])
            
            #Vector with the 2 APs related
            m= cor[2:3, j]
            
            #Adds the names of the AP related with it
            x$Relations[[n[1]]] <-c(x$Relations[[n[1]]], m[!m %in% x$AP[n[1]]])
      
            x$Relations[[n[2]]] <-c(x$Relations[[n[2]]], m[!m %in% x$AP[n[2]]])
            
            x$Relations[[n[1]]] <- unique(x$Relations[[n[1]]])
            x$Relations[[n[2]]] <- unique(x$Relations[[n[2]]])
            
            }
        
        }
      
      }
      
      return (as.data.frame(x))
    
      }

    }
  
}



#Global variables to configure######################################################################

#Set the scale of the y axis in the histograms
yscale <-0.8

#Set path where the file with all the capture from wireshark is
path <- "Portatil/cas13_500"

#Sets the the interval of the correlations between 2 APs (has to be bigger than 1)
cortime<-5

#limitcor
limitcor=-0.3

#type TRUE if you want the plots as histograms or FALSE if you want a line in time
plothist= TRUE

#Calling functions############################################################

#first

data <- export_aps_data(path)
capture <- data$capture
aps <- data$aps

#RSSI

rssi()

#second

Channel.Data <- channel_data(capture)


#third

aps <-extended_aps_table()

#forth

sta<- sta_table()

#fifth

apinfo<-ap_histograms(yscale)

#sixth

for (i in 1:14){
  
  assign(paste0("Cor", i), total_correlation(i))

}

#seventh

for (i in 1:14){
  
  x <- correlation_xseg(i, cortime)
  
  if(!is.null(x) && ncol(x) > 1){
  
  assign(paste0("segCor", i), x)
  
  }
  rm(x)

}


#eighth

for (i in 1:14){
  
  y <- Relation(i, limitcor)
  
  if(!is.null(y)){
    
    assign(paste0("Relations", i), y)
    
  }
  rm(y)
  
}


