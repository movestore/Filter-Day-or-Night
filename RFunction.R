library('move2')
library('foreach')
library('suncalc')
library('sf')

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
    data.split <- split(data,mt_track_id(data))
    data.night <- foreach(datai = data.split) %do% {
      logger.info(unique(mt_track_id(datai)))
      timecooi <- data.frame(as.Date(mt_time(datai)),st_coordinates(datai))
      names(timecooi) <- c("date","lon","lat") 
      sunupx <- getSunlightTimes(data=timecooi,keep=c("sunrise"))$sunrise + upX*60
      sundownx <- getSunlightTimes(data=timecooi,keep=c("sunset"))$sunset + downX*60
      
      daynight <- rep(NA,length(sunupx)) #add attribute day/night
      daynight[which(mt_time(datai)<sunupx | mt_time(datai)>sundownx)] <- "night"
      daynight[which(mt_time(datai)>sunupx & mt_time(datai)<sundownx)] <- "day"
      
      datai$sunupx <- sunupx
      datai$sundownx <- sundownx
      datai$daynight <- daynight

      # there are no sunup or sundown during Arctic summer, then only day positions possible "sunupdown". respectively for Arctic winter
      ix <- which(is.na(sunupx) | is.na(sundownx))
      
      ix_ArcSum <- ix[st_coordinates(datai)[ix,2]>50 & as.POSIXlt(mt_time(datai[ix,]))$mon %in% c(4:8)]
      ix_ArcWin <- ix[st_coordinates(datai)[ix,2]>50 & as.POSIXlt(mt_time(datai[ix,]))$mon %in% c(10:11,0:2)]
      ix_AntWin <- ix[st_coordinates(datai)[ix,2]<(-50) & as.POSIXlt(mt_time(datai[ix,]))$mon %in% c(4:8)]
      ix_AntSum <- ix[st_coordinates(datai)[ix,2]<(-50) & as.POSIXlt(mt_time(datai[ix,]))$mon %in% c(10:11,0:2)]
      
      if (length(ix_ArcSum>0) | length(ix_AntSum)>0) datai$daynight[c(ix_ArcSum,ix_AntSum)] <- "day" #fill NA daynight for arctic summer/winter
      if (length(ix_ArcWin>0) | length(ix_AntWin)>0) datai$daynight[c(ix_ArcWin,ix_AntWin)] <- "night"
      
      if (is.null(window))
      {
        data.nighti <- datai
        year <- as.POSIXlt(mt_time(data.nighti))$year+1900
        yday <- as.POSIXlt(mt_time(data.nighti))$yday
        data.nighti$year <- year
        data.nighti$yday <- yday
      } else
      {
        if (window=="sundownup") #night 
        {
          if (length(ix_ArcSum)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) are taken out for the calculations."))
            datai <- datai[-ix_ArcSum,]
          }
          if (length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntSum)," southern summer positions without 'night' (Nov-Mar) are taken out for the calculations."))
            datai <- datai[-ix_AntSum,]
          }
          
          if (length(ix_ArcWin)>0 & length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions above the Arctic circle and below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) and", length(ix_AntWin), " southern winter positions without 'day' (May-Sep) are kept in the data fully."))
            datai.ND <- datai[-c(ix_ArcWin,ix_AntWin),]
            ix.ND <- seq(along=datai)[-c(ix_ArcWin,ix_AntWin)]
            selND <- which(mt_time(datai.ND)<=datai.ND$sunupx | mt_time(datai.ND)>=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_ArcWin,ix_AntWin)),]
          } else if (length(ix_ArcWin)>0 & length(ix_AntWin)==0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) are kept in the data fully."))
            datai.ND <- datai[-c(ix_ArcWin),]
            ix.ND <- seq(along=datai)[-c(ix_ArcWin)]
            selND <- which(mt_time(datai.ND)<=datai.ND$sunupx | mt_time(datai.ND)>=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_ArcWin)),]
          } else if (length(ix_ArcWin)==0 & length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntWin)," southern winter positions without 'day' (May-Sep) are kept in the data fully."))
            datai.ND <- datai[-c(ix_AntWin),]
            ix.ND <- seq(along=datai)[-c(ix_AntWin)]
            selND <- which(mt_time(datai.ND)<=datai.ND$sunupx | mt_time(datai.ND)>=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_AntWin)),]
          } else data.nighti <- datai[mt_time(datai)<=datai$sunupx | mt_time(datai)>=datai$sundownx,]
          
          year <- as.POSIXlt(mt_time(data.nighti))$year+1900
          yday <- as.POSIXlt(mt_time(data.nighti))$yday
          ynight <- yday
          
          ixx <- which(is.na(data.nighti$sundownx))
          if (length(ixx)>0)
          {
            ynight[mt_time(data.nighti[-ixx])>data.nighti$sundownx[-ixx]] <- ynight[mt_time(data.nighti[-ixx])>data.nighti$sundownx[-ixx]]+1
            
            # for Arctic/Antarctic nights the night goes from midday to midday, which depends on the location..
            midday_ixx <- solarnoon(coordinates(data.nighti[ixx]),mt_time(data.nighti[ixx]),POSIXct.out=TRUE)$time
            ynight[mt_time(data.nighti[ixx])>midday_ixx] <- ynight[mt_time(data.nighti[ixx])>midday_ixx]+1
          } else ynight[mt_time(data.nighti)>data.nighti$sundownx] <- ynight[mt_time(data.nighti)>data.nighti$sundownx]+1
          
          # adapt for New Year's Eve
          year[as.POSIXlt(mt_time(data.nighti))$mday==31 & as.POSIXlt(mt_time(data.nighti))$mon==11 & mt_time(data.nighti)>data.nighti$sundownx] <- year[as.POSIXlt(mt_time(data.nighti))$mday==31 & as.POSIXlt(mt_time(data.nighti))$mon==11 & mt_time(data.nighti)>data.nighti$sundownx]+1
          ynight[as.POSIXlt(mt_time(data.nighti))$mday==31 & as.POSIXlt(mt_time(data.nighti))$mon==11 & mt_time(data.nighti)>data.nighti$sundownx] <- 0
          
          data.nighti$year <- year
          data.nighti$yday <- yday
          data.nighti$ynight <- ynight
        }
        
        if (window=="sunupdown") # day
        {
          if (length(ix_ArcWin)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) are taken out for the calculations."))
            datai <- datai[-ix_ArcWin,]
          }
          if (length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntWin)," southern winter positions without 'day' (May-Sep) are taken out for the calculations."))
            datai <- datai[-ix_AntWin,]
          }
          
          if (length(ix_ArcSum)>0 & length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions above the Arctic circle and below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) and", length(ix_AntSum), " southern summer positions without 'night' (Nov-Mar) are kept in the data fully."))
            datai.ND <- datai[-c(ix_ArcSum,ix_AntSum),]
            ix.ND <- seq(along=datai)[-c(ix_ArcSum,ix_AntSum)]
            selND <- which(mt_time(datai.ND)>=datai.ND$sunupx & mt_time(datai.ND)<=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_ArcSum,ix_AntSum)),]
          } else if (length(ix_ArcSum)>0 & length(ix_AntSum)==0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) are kept in the data fully."))
            datai.ND <- datai[-c(ix_ArcSum),]
            ix.ND <- seq(along=datai)[-c(ix_ArcSum)]
            selND <- which(mt_time(datai.ND)>=datai.ND$sunupx & mt_time(datai.ND)<=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_ArcSum)),]
          } else if (length(ix_ArcSum)==0 & length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(datai))," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntSum)," southern summer positions without 'night' (Nov-Mar) are kept in the data fully."))
            datai.ND <- datai[-c(ix_AntSum),]
            ix.ND <- seq(along=datai)[-c(ix_AntSum)]
            selND <- which(mt_time(datai.ND)>=datai.ND$sunupx & mt_time(datai.ND)<=datai.ND$sundownx)
            data.nighti <- datai[sort(c(ix.ND[selND],ix_AntSum)),]
          } else data.nighti <- datai[mt_time(datai)>=datai$sunupx & mt_time(datai)<=datai$sundownx,]
          
          year <- as.POSIXlt(mt_time(data.nighti))$year+1900
          yday <- as.POSIXlt(mt_time(data.nighti))$yday
          data.nighti$year <- year
          data.nighti$yday <- yday
        }
      }
      return(data.nighti)
    }
    names (data.night) <- names(data.split)
    data.night.nozero <- data.night[unlist(lapply(data.night, length) > 0)] #even if move2 allows empty tracks, remove them here
    
    if (length(data.night.nozero)==0) 
    {
      logger.info("Your data contain no night/day positions. No csv overview saved.")
      result_sk <- mt_stack(data.night.nozero,.track_combine="rename")
      result <- result_sk |> dplyr::arrange(mt_track_id(result_sk),mt_time(result_sk)) ## for some reason at some point sometimes data get unsorted
    } else 
    {
      result_sk <- mt_stack(data.night.nozero,.track_combine="rename")
      result <- result_sk |> dplyr::arrange(mt_track_id(result_sk),mt_time(result_sk)) ## for some reason at some point sometimes data get unsorted
      data.night.df <- as.data.frame(result)
      na.row.ix <- which(apply(data.night.df,2,function (x) all(is.na(x))))
      if (length(na.row.ix)>0) data.night.df.nna <- data.night.df[,-na.row.ix] else data.night.df.nna <- data.night.df #remove columns with all NA, fix to also work if no na rows exist
      
      write.csv(data.night.df.nna,file = appArtifactPath("data_selectedTime_night_day.csv"),row.names=FALSE) #csv artefakt of all night (or day, or all...) positions
    }

  return(result)
}

  
  
  
  
  
  
  
  
  
  
