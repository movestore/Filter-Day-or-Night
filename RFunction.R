library('move')
library('foreach')
library('maptools')

rFunction <- function(data, window=NULL, upX=0, downX=0)
{
  Sys.setenv(tz="UTC")
  
  if (is.null(window)) 
  {
    logger.info("You have not selected if to extract positions during night or day. The App is now returning the full data set with sunriset and day/night as additional attributes.")
  } else
  {
    if (window=="sundownup") 
    {
      logger.info(paste("Your have selected to extract positions from sunset +",downX,"minutes until sunrise +",upX,"minutes. The algorithm starts from the sunrise side back."))
    } else if (window=="sunupdown") 
    {
      logger.info(paste("Your have selected to extract positions from sunrise +",upX,"minutes until sunset +",downX,"minutes. The algorithm starts from the sunset side back."))
    } else
    {
      logger.info("Your selected day/night selection option is not valid. Adapt your settings, the App is now returning the full data set.")
      window <- NULL
    }
  }
  
    # select night or day positions (call it "night")
    data.split <- move::split(data)
    data.night <- foreach(datai = data.split) %do% {
      print(namesIndiv(datai))
      sunupx <- data.frame(sunriset(coordinates(datai), timestamps(datai), direction="sunrise", POSIXct.out=TRUE))$time + upX*60
      sundownx <- data.frame(sunriset(coordinates(datai), timestamps(datai), direction="sunset", POSIXct.out=TRUE))$time + downX*60
      daynight <- rep("night",length(sunupx)) #add attribute day/night
      daynight[which(timestamps(datai)>sunupx & timestamps(datai)<sundownx)] <- "day"
      datai@data <- cbind(datai@data,sunupx,sundownx,daynight)
      
      # there are no sunup or sundown during Arctic summer, then only day positions possible "sunupdown". respectively for Arctic winter
      ix <- which(is.na(sunupx) | is.na(sundownx))
      
      ix_ArcSum <- ix[coordinates(datai)[ix,2]>50 & as.POSIXlt(timestamps(datai[ix,]))$mon %in% c(4:8)]
      ix_ArcWin <- ix[coordinates(datai)[ix,2]>50 & as.POSIXlt(timestamps(datai[ix,]))$mon %in% c(10:11,0:2)]
      ix_AntWin <- ix[coordinates(datai)[ix,2]<(-50) & as.POSIXlt(timestamps(datai[ix,]))$mon %in% c(4:8)]
      ix_AntSum <- ix[coordinates(datai)[ix,2]<(-50) & as.POSIXlt(timestamps(datai[ix,]))$mon %in% c(10:11,0:2)]
      
      if (length(ix_ArcSum>0) | length(ix_AntSum)>0) datai@data$daynight[c(ix_ArcSum,ix_AntSum)] <- "day" #fill NA daynight for arctic summer/winter
      if (length(ix_ArcWin>0) | length(ix_AntWin)>0) datai@data$daynight[c(ix_ArcWin,ix_AntWin)] <- "night"
      
      if (is.null(window))
      {
        data.nighti <- datai
        year <- as.POSIXlt(timestamps(data.nighti))$year+1900
        yday <- as.POSIXlt(timestamps(data.nighti))$yday
        data.nighti@data <- cbind(data.nighti@data,year,yday)
      } else
      {
        if (window=="sundownup") #night 
        {
          if (length(ix_ArcSum)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) are taken out for the calculations."))
            datai <- datai[-ix_ArcSum,]
          }
          if (length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntSum)," southern summer positions without 'night' (Nov-Mar) are taken out for the calculations."))
            datai <- datai[-ix_AntSum,]
          }
          
          if (length(ix_ArcWin)>0 & length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions above the Arctic circle and below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) and", length(ix_AntWin), " southern winter positions without 'day' (May-Sep) are kept in the data fully."))
            datai.ND <- datai[-c(ix_ArcWin,ix_AntWin),]
            ix.ND <- seq(along=datai)[-c(ix_ArcWin,ix_AntWin)]
            selND <- which(timestamps(datai.ND)<=datai.ND$sunupx | timestamps(datai.ND)>=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_ArcWin,ix_AntWin)),]
          } else if (length(ix_ArcWin)>0 & length(ix_AntWin)==0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) are kept in the data fully."))
            datai.ND <- datai[-c(ix_ArcWin),]
            ix.ND <- seq(along=datai)[-c(ix_ArcWin)]
            selND <- which(timestamps(datai.ND)<=datai.ND$sunupx | timestamps(datai.ND)>=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_ArcWin)),]
          } else if (length(ix_ArcWin)==0 & length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntWin)," southern winter positions without 'day' (May-Sep) are kept in the data fully."))
            datai.ND <- datai[-c(ix_AntWin),]
            ix.ND <- seq(along=datai)[-c(ix_AntWin)]
            selND <- which(timestamps(datai.ND)<=datai.ND$sunupx | timestamps(datai.ND)>=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_AntWin)),]
          } else data.nighti <- datai[timestamps(datai)<=datai$sunupx | timestamps(datai)>=datai$sundownx,]
          
          year <- as.POSIXlt(timestamps(data.nighti))$year+1900
          yday <- as.POSIXlt(timestamps(data.nighti))$yday
          ynight <- yday
          
          ixx <- which(is.na(data.nighti$sundownx))
          if (length(ixx)>0)
          {
            ynight[timestamps(data.nighti[-ixx])>data.nighti$sundownx[-ixx]] <- ynight[timestamps(data.nighti[-ixx])>data.nighti$sundownx[-ixx]]+1
            
            # for Arctic/Antarctic nights the night goes from midday to midday, which depends on the location..
            midday_ixx <- solarnoon(coordinates(data.nighti[ixx]),timestamps(data.nighti[ixx]),POSIXct.out=TRUE)$time
            ynight[timestamps(data.nighti[ixx])>midday_ixx] <- ynight[timestamps(data.nighti[ixx])>midday_ixx]+1
          } else ynight[timestamps(data.nighti)>data.nighti$sundownx] <- ynight[timestamps(data.nighti)>data.nighti$sundownx]+1
          
          # adapt for New Year's Eve
          year[as.POSIXlt(timestamps(data.nighti))$mday==31 & as.POSIXlt(timestamps(data.nighti))$mon==11 & timestamps(data.nighti)>data.nighti$sundownx] <- year[as.POSIXlt(timestamps(data.nighti))$mday==31 & as.POSIXlt(timestamps(data.nighti))$mon==11 & timestamps(data.nighti)>data.nighti$sundownx]+1
          ynight[as.POSIXlt(timestamps(data.nighti))$mday==31 & as.POSIXlt(timestamps(data.nighti))$mon==11 & timestamps(data.nighti)>data.nighti$sundownx] <- 0
          
          data.nighti@data <- cbind(data.nighti@data,year,yday,ynight)
        }
        
        if (window=="sunupdown") # day
        {
          if (length(ix_ArcWin)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) are taken out for the calculations."))
            datai <- datai[-ix_ArcWin,]
          }
          if (length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntWin)," southern winter positions without 'day' (May-Sep) are taken out for the calculations."))
            datai <- datai[-ix_AntWin,]
          }
          
          if (length(ix_ArcSum)>0 & length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions above the Arctic circle and below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) and", length(ix_AntSum), " southern summer positions without 'night' (Nov-Mar) are kept in the data fully."))
            datai.ND <- datai[-c(ix_ArcSum,ix_AntSum),]
            ix.ND <- seq(along=datai)[-c(ix_ArcSum,ix_AntSum)]
            selND <- which(timestamps(datai.ND)>=datai.ND$sunupx & timestamps(datai.ND)<=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_ArcSum,ix_AntSum)),]
          } else if (length(ix_ArcSum)>0 & length(ix_AntSum)==0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) are kept in the data fully."))
            datai.ND <- datai[-c(ix_ArcSum),]
            ix.ND <- seq(along=datai)[-c(ix_ArcSum)]
            selND <- which(timestamps(datai.ND)>=datai.ND$sunupx & timestamps(datai.ND)<=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_ArcSum)),]
          } else if (length(ix_ArcSum)==0 & length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(datai)," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntSum)," southern summer positions without 'night' (Nov-Mar) are kept in the data fully."))
            datai.ND <- datai[-c(ix_AntSum),]
            ix.ND <- seq(along=datai)[-c(ix_AntSum)]
            selND <- which(timestamps(datai.ND)>=datai.ND$sunupx & timestamps(datai.ND)<=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_AntSum)),]
          } else data.nighti <- datai[timestamps(datai)>=datai$sunupx & timestamps(datai)<=datai$sundownx,]
          
          year <- as.POSIXlt(timestamps(data.nighti))$year+1900
          yday <- as.POSIXlt(timestamps(data.nighti))$yday
          data.nighti@data <- cbind(data.nighti@data,year,yday)
        }
      }
      return(data.nighti)
    }
    names (data.night) <- names(data.split)
    data.night.nozero <- data.night[unlist(lapply(data.night, length) > 0)]
    
    if (length(data.night.nozero)==0) 
    {
      logger.info("Your data contain no night/day positions. No csv overview saved. Return NULL.")
      result <- NULL
    } else 
    {
      result <- moveStack(data.night.nozero)
      data.night.df <- as.data.frame(result)
      data.night.df.nna <- data.night.df[,-which(apply(data.night.df,2,function (x) all(is.na(x))))] #remove columns with all NA
      
      write.csv(data.night.df.nna,file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"data_selectedTime_night_day.csv"),row.names=FALSE) #csv artefakt of all night (or day...) positions
      #write.csv(data.night.df.nna,file = "data_selectedTime_night_day.csv",row.names=FALSE)
    }

  return(result)
}

  
  
  
  
  
  
  
  
  
  
