rm(list=ls())

require(ggplot2)
require(openxlsx)
require(plyr)
require(lubridate)
library(reshape2)
library(skimr)
library(dplyr)
#library(sjPlot)
library(table1)
library(xtable)

baseWD <- "C:/Users/Luke/Desktop/competitiveMPEC_1.16"
setwd(paste(baseWD, "", sep="/"))

## Load model results ####

sec_in_min = 60 #60 seconds in a minute
#configure RT case slice per hour
rt_da_resolution = 12 #number of rt tmps in each da tmp
tmps = 48
slicer = 6

# helper function to read all files in date range of a specific output
readFiles <- function(filename, dates, dateWD, subFolder="results_DA"){
  
  for(i in 1:length(dates)){
    date <- dates[i]
    dateString <- paste(as.character(format(date, "%m")), as.character(format(date, "%d")), as.numeric(format(date, "%Y")), sep=".")
    setwd(paste(dateWD, dateString, subFolder, sep="/"))
    
    # load file
    df <- read.csv(filename)
    df$date <- date
    
    if(i == 1){
      fullDF <- df
    } else{
      fullDF <- rbind(fullDF, df)
    }
  }
  return(fullDF)
}

readFilesRT <- function(filename, dates, dateWD){
  for(i in 1:length(dates)){
    for(j in 1:slicer){
      date <- dates[i]
      subFolder <- paste("results_RT", tmps * (j - 1) + 1, tmps * j, sep="_")
      dateString <- paste(as.character(format(date, "%m")), as.character(format(date, "%d")), as.numeric(format(date, "%Y")), sep=".")
      setwd(paste(dateWD, dateString, subFolder, sep="/"))
    
      # load file
      dftmp <- read.csv(filename)
      dftmp$date <- date
      if(j == 1){
        df <- dftmp
      } else{
        df <- rbind(df, dftmp)
        }
      }
    if(i == 1){
      fullDF <- df
    } else{
      fullDF <- rbind(fullDF, df)
    }
  }
  return(fullDF)
}


loadResults <- function(dates,folder,subfolder){
  dateMonth <- unique(format(dates, "%b"))  # extract month of dates for directory
  dateYear <- unique(format(dates, "%Y"))  # extract year of dates for directory
  dateResultsWD<-paste(baseWD, folder, sep="/")

  modelLMP <- readFiles("zonal_prices.csv", dates, dateResultsWD,subFolder=subfolder)
  txFlows <- readFiles("tx_flows.csv", dates, dateResultsWD,subFolder=subfolder)
  offer <- readFiles("generator_segment_offer.csv", dates, dateResultsWD,subFolder=subfolder)
  nucoffer <- readFiles("nuc_offer.csv", dates, dateResultsWD,subFolder=subfolder)
  dispatch <- readFiles("generator_dispatch.csv", dates, dateResultsWD,subFolder=subfolder)
  storage <- readFiles("storage_dispatch.csv", dates, dateResultsWD,subFolder=subfolder)
  objective <- readFiles("objective.csv",dates, dateResultsWD,subFolder=subfolder)
  #VRE <- readFiles("renewable_generation.csv", dates, dateResultsWD)
  
  #ordc <- readFiles("full_ordc.csv", dates, dateResultsWD, subFolder="inputs")
  gens <- readFiles("generators_descriptive.csv", dates, dateResultsWD, subFolder="inputs")
  zonalLoad <- readFiles("timepoints_zonal.csv", dates, dateResultsWD, subFolder="inputs")
  zonalLoadRT <- readFiles("timepoints_zonal_rt_da_tmp.csv",dates,dateResultsWD,subFolder="inputs")
  storageresources <- readFiles("storage_resources.csv", dates, dateResultsWD, subFolder="inputs")
  generatorresources <- readFiles("generators.csv", dates, dateResultsWD, subFolder="inputs")
  #emissions <- readFiles("generator_segment_marginalcost.csv", dates, dateResultsWD, subFolder="inputs")
  generators_avail <- readFiles("generators_scheduled_availability.csv",dates,dateResultsWD,subFolder="inputs")
  generators_avail_rt <- readFiles("generators_scheduled_availability_rt_da_tmp.csv",dates,dateResultsWD,subFolder="inputs")
  
  #clean and format
  modelLMP <- AddDatetime(modelLMP)
  storage <- AddDatetime(storage)
  dispatch <- AddDatetime(dispatch)
  
  # return results
  results <- list(modelLMP, zonalLoad,zonalLoadRT, dispatch, gens, txFlows, storage, offer, nucoffer, storageresources, generatorresources, objective,
                  generators_avail,generators_avail_rt)
  names(results) <- c("modelLMP", "zonalLoad","zonalLoadRT", "dispatch", "gens", "txFlows", "storage", "offer", "nucoffer", "storageresources", "generatorresources", "objective",
                      "generatoravail","generatoravailrt")
  return(results)
}

loadResultsRT <- function(dates,folder){
  dateMonth <- unique(format(dates, "%b"))  # extract month of dates for directory
  dateYear <- unique(format(dates, "%Y"))  # extract year of dates for directory
  dateResultsWD<-paste(baseWD, folder, sep="/")

  gens <- readFiles("generators_descriptive.csv", dates, dateResultsWD, subFolder="inputs")
  zonalLoad <- readFiles("timepoints_zonal.csv", dates, dateResultsWD, subFolder="inputs")
  storageresources <- readFiles("storage_resources.csv", dates, dateResultsWD, subFolder="inputs")
  generatorresources <- readFiles("generators.csv", dates, dateResultsWD, subFolder="inputs")
  modelLMPRT <- readFilesRT("zonal_prices.csv", dates, dateResultsWD)
  txFlowsRT <- readFilesRT("tx_flows.csv", dates, dateResultsWD)
  offerRT <- readFilesRT("generator_segment_offer.csv", dates, dateResultsWD)
  nucofferRT <- readFilesRT("nuc_offer.csv", dates, dateResultsWD)
  dispatchRT <- readFilesRT("generator_dispatch.csv", dates, dateResultsWD)
  storageRT <- readFilesRT("storage_dispatch.csv", dates, dateResultsWD)
  objectiveRT <- readFilesRT("objective.csv",dates,dateResultsWD)
  
  #clean and format
  modelLMPRT <- AddDatetime(modelLMPRT)
  storageRT <- AddDatetime(storageRT)
  dispatchRT <- AddDatetime(dispatchRT)
  
  # return resultsRT
  resultsRT <- list(modelLMPRT, zonalLoad, dispatchRT, gens, txFlowsRT, storageRT, offerRT, nucofferRT, storageresources, generatorresources, objectiveRT)
  names(resultsRT) <- c("modelLMP", "zonalLoad", "dispatch", "gens", "txFlows", "storage", "offer", "nucoffer", "storageresources", "generatorresources", "objective")
  return(resultsRT)
}

# helper function to load data from all cases
loadAllCases <- function(dates,folder="303.301SS_Wind303"){
  results <- loadResults(dates,folder)
  return(results,resultsRT)
}

# helper function to add datetime to dataframe and reformat to target resolution (5-min default)
AddDatetime <- function(df,BaseResolution=60,TargetResolution=5){
  colnames(df)[2] <- "hour" #kludgy for now
  NativeResolution <- 60/(max(df$hour)/24)
  Ratio <- BaseResolution/NativeResolution
  tmp_offsets <- seq(-55*sec_in_min, 0, 5*sec_in_min)
  
  df$time <- paste(df$hour%/%Ratio,df$hour%%Ratio*NativeResolution,sep=":")
  df$datetime <- as.POSIXct(with(df, paste(date, time)), format = "%Y-%m-%d %H:%M")
  
  df <- df[rep(seq_len(nrow(df)), each = NativeResolution/TargetResolution), ]#repeat to get to RT
  df$datetime <- df$datetime + c(diff(df$datetime)==0,F)*rep(tmp_offsets,length(df$datetime)/length(tmp_offsets))
  #special conditional for storage to rescale
  if("charge" %in% colnames(df)){
    df$charge <- df$charge*Ratio
    df$discharge <- df$discharge*Ratio
    df$profit <- df$profit*TargetResolution/NativeResolution
  }
  if("GeneratorDispatch" %in% colnames(df)){
    df$GeneratorDispatch <- df$GeneratorDispatch*Ratio
  }
  
  return(df)
}

aggregateCaseData <- function(results, targetData){
  newDF <- data.frame()
  # format results by case
  for(case in names(results)){
    dfTemp <- results[[case]][[targetData]] 
    dfTemp$case <- case
    newDF <- rbind(newDF, dfTemp)
  }
  return(newDF)
}

plotPrices <- function(results,dates,plotTitle,hours=24){
  prices <- results[['modelLMP']]
  prices$zone <- substr(prices[,1],start=1,stop=1)
  prices$zone <- paste0("Area ",prices$zone)
  prices$busID <- substr(prices[,1],start=2,stop=3)
  prices$X <- as.character(prices$X)
  
  #Luke's plotting code (active)
  c25 <- c(
    "dodgerblue2", "#E31A1C", # red
    "green4",
    "#6A3D9A", # purple
    "#FF7F00", # orange
    "gray70", "gold1",
    "skyblue2", "#FB9A99", # lt pink
    "palegreen2",
    "#CAB2D6", # lt purple
    "#FDBF6F", # lt orange
    "black", "khaki2",
    "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
    "darkturquoise", "green1", "yellow4", "yellow3",
    "darkorange4", "brown"
  )
  #pie(rep(1, 25), col = c25)
  #scale_color_manual(values=c25) +
  #facet_wrap(~zone, nrow=1) + 
  ggplot(data=prices, aes(x=datetime, y=LMP, color=busID)) + geom_line(lwd=1.5) +
    theme_classic() + ylab("$/MWh") + xlab("") +
    scale_x_datetime() +
    guides(colour=guide_legend(title="Bus: ", nrow=3))+
    theme(legend.text = element_text(size=24),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=24),
          strip.text.x = element_text(size = 24)) +
    ggtitle(paste("LMP by Bus for", plotTitle))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("prices ", plotTitle, ".png"),dpi=500, width=16, height=6)
  
  return(prices)
}

compareplotPrices <- function(prices_df1,prices_df2){

  pricesdelta_df <- merge(prices_df1, prices_df2, by = c("datetime","zone","busID"))
  pricesdelta_df$LMP <- pricesdelta_df$LMP.x - pricesdelta_df$LMP.y
  
  #Luke's plotting code (active)
  ggplot(data=pricesdelta_df, aes(x=datetime, y=LMP, color=busID)) + geom_line(lwd=1.5) + 
    facet_wrap(~zone, nrow=1) + 
    theme_classic() + ylab("$/MWh") + xlab("") +
    scale_x_datetime() +
    guides(colour=guide_legend(title="Bus: ", nrow=5))+
    theme(legend.text = element_text(size=24),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=16),
          axis.text.y= element_text(size=16),
          strip.text.x = element_text(size = 24)) +
    ggtitle(paste("Delta LMP by Bus"))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("prices delta",".png"), width=20, height=8)
}

plotDispatch <- function(results, dates, plotTitle, hours=24){
  
  dispatch <- results[["dispatch"]]
  gens <- results[["gens"]]
  offer <- results[['offer']]

  offer$segID <- gsub("[[:print:]]*-", "", offer[,1])
  offer$genID <- gsub("-\\d+","",offer[,1])
  offer$area <- substr(offer$Zone,start=1,stop=3)

  #offer <- merge(offer, gens[,c("Name", "Category")], by.x="genID", by.y="Name", all.x=T)

  #drop duplicated entries in output
  #offer <- offer[!duplicated(offer), ]
  # summarize by fuel type
  #fuelemissions <- ddply(offer, ~ date + hour + area, summarise, Emissions = sum(SegmentEmissions))
  #fuelemissions$area <- factor(fuelemissions$area)
  #fuelemissions <- arrange(fuelemissions,area,hour)
  #fuelemissions <- AddDatetime(fuelemissions)
  
  # calculate all zones
  #all_emissions <- ddply(fuelemissions, ~ datetime, summarise, Emissions = sum(Emissions))
  #all_emissions$area <- "All Zones"

  #fuelemissions <- fuelemissions[,c("datetime", "Emissions", "area")]
  #fuelemissions <- rbind(fuelemissions, all_emissions)
  #colnames(fuelemissions) <- c("datetime","Emissions","zone")
  #fuelemissions$zone <- as.factor(fuelemissions$zone)
  #fuelemissions$Category <- "Coal"
  print('dispatch place reached')
  dispatch$zone <- substr(dispatch[,1],start=1,stop=3)
  
  # match with fuel type
  dispatch <- merge(dispatch, gens[,c("Name", "Category")], by.x="X", by.y="Name", all.x=T)
  print('merged')
  #drop duplicated entries in output
  dispatch <- dispatch[!duplicated(dispatch), ]
  
  # summarize by fuel type
  #fuelDispatch <- ddply(dispatch, ~ datetime + zone + Category, summarise, MW = sum(GeneratorDispatch))
  fuelDispatch <- dispatch %>% group_by(datetime,zone,Category) %>% summarise(MW = sum(GeneratorDispatch))
  fuelDispatch$zone <- factor(fuelDispatch$zone)
  print('dplyed')
  fuelDispatch$Category <- factor(fuelDispatch$Category, levels = c("Solar RTPV","Solar PV","Wind",
                                                                    "Oil ST","Oil CT","Gas CT","Gas CC",
                                                                    "Coal","Hydro"))
  #create color panel for later use
  dispatchcolors <- c("yellow","yellow","cyan",
                      "green","green","brown","orange","grey","blue")
  
  #drop NA's
  fuelDispatch <- fuelDispatch[!is.na(fuelDispatch$Category),]
  
  # calculate all zones
  #all_dispatch <- ddply(fuelDispatch, ~ datetime + Category, summarise, MW = sum(MW))
  all_dispatch <- fuelDispatch %>% group_by(datetime,Category) %>% summarise(MW=sum(MW))
  all_dispatch$zone <- "All Zones"
  
  fuelDispatch <- fuelDispatch[,c("datetime", "Category", "MW", "zone")]
  fuelDispatch <- rbind(fuelDispatch, all_dispatch)
  print('progress')
 
  #aggregate by month-hour average
  all_dispatch$month <- month(all_dispatch$datetime)
  all_dispatch$hour <- hour(all_dispatch$datetime) #check to make sure 2020
  
  all_monthhour <- all_dispatch %>% group_by(month,hour,Category) %>% summarise(MW = mean(MW))
  #create a frame for load
  all_load <- all_monthhour%>%group_by(month,hour) %>% summarise(LoadMW=sum(MW)) 
  notVRE <- c("Oil ST","Oil CT","Gas CT","Gas CC",
           "Coal","Hydro")
  all_netload <- all_monthhour[all_monthhour$Category%in%notVRE,] %>%group_by(month,hour) %>% summarise(LoadMW=sum(MW))
  #create a column for net load
  
  all_monthhour$monthhour <- paste0(all_monthhour$month,"-",all_monthhour$hour)
  all_load$monthhour <- paste0(all_load$month,"-",all_load$hour)
  all_netload$monthhour <- paste0(all_netload$month,"-",all_netload$hour)
  print('and...plotting')
  
  ggplot(data=all_monthhour,aes(x=hour,y=MW,fill=Category)) + geom_area()+
    geom_line(data=all_load,aes(x=hour,y=LoadMW),color="black",lwd=3,inherit.aes = FALSE) +
    geom_line(data=all_netload,aes(x=hour,y=LoadMW),color="black",lwd=3,linetype="dashed",inherit.aes = FALSE) +
    theme_classic()+ylab("Generation or Load (MW)")+
    guides(fill=guide_legend(title="", nrow=2, byrow=TRUE)) +
    xlab("Hour Beginning") + scale_fill_manual(values=dispatchcolors) +
    theme(legend.title = element_text(size=24),
          legend.text = element_text(size=22),
          legend.position = "bottom",
          plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),
          axis.title.y = element_text(size=28),
          axis.title.x = element_text(size=28),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20))+
    ggtitle("Hourly Average Generation and Load")
  
  #Luke's plotting code (inactive)
  #ggplot(data=fuelDispatch, aes(x=datetime, y=MW, fill=Category)) + geom_area() + 
  #  geom_line(data = fuelemissions, aes(x=datetime, y = Emissions,linetype=''),colour='black',size=2) +
  #  facet_wrap(~zone, nrow=3, scales = "free") +
  #  theme_classic() + ylab("Gen (MW) or Emissions (tonne)") +
  #  guides(fill=guide_legend(title="Gen: ", nrow=2, byrow=TRUE),
  #         linetype=guide_legend(title="Emissions: ")) +
  #  xlab("") + scale_x_datetime() + scale_fill_manual(values=dispatchcolors) +
  #  theme(legend.title = element_text(size=32),
  #        legend.text = element_text(size=28),
  #        legend.position = "bottom",
  #        plot.title = element_text(size = 40, face = 'bold', hjust = 0.5),
  #        axis.title.y = element_text(size=32),
  #        axis.text.x= element_text(size=8),
  #        axis.text.y= element_text(size=16),
  #        strip.text.x = element_text(size = 24)) +
  #  ggtitle(paste("Generation by fuel for", plotTitle))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("dispatch ", plotTitle, ".png"), width=10, height=6)
  
  return(fuelDispatch)
}

compareplotDispatch <- function(dispatch_df1,dispatch_df2){
  
  dispatchdelta_df <- merge(dispatch_df1, dispatch_df2, by = c("datetime","Category","zone"))
  dispatchdelta_df$MW <- dispatchdelta_df$MW.x - dispatchdelta_df$MW.y
  
  dispatchdelta_df$Category <- factor(dispatchdelta_df$Category, levels = c("Solar RTPV","Solar PV","Wind",
                                                                            "Oil ST","Oil CT","Gas CT","Gas CC",
                                                                            "Coal","Hydro"))
  #create color panel for later use
  dispatchcolors <- c("yellow","yellow","cyan",
                      "green","green","brown","orange","grey","blue")
  
  
  #Luke's plotting code (active)
  ggplot(data=dispatchdelta_df, aes(x=datetime, y=MW, fill=Category)) + geom_area() + facet_wrap(~zone, nrow=3, scales = "free") + 
    theme_classic() + ylab("MW") + guides(fill=guide_legend(title="", nrow=2, byrow=TRUE)) + xlab("") +
    scale_x_datetime() + scale_fill_manual(values=dispatchcolors) +
    theme(legend.text = element_text(size=32),
          legend.position = "bottom",
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=16),
          axis.text.y= element_text(size=16),
          strip.text.x = element_text(size = 24)) +
    ggtitle(paste("Generation by fuel deltas"))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("dispatch delta", ".png"), width=20, height=12)
}

compareTotalEmissions <- function(emissiondflist,plotTitle='hi',resolution=NA){
    #eventually probably a list of dfs as input
    print(names(emissiondflist))
    for (i in 1:length(emissiondflist)){
      emissiondflist[[i]]$label <- names(emissiondflist[i]) #may want better label
    }
    emissionsdf <- do.call("rbind", emissiondflist)
    
    #print(emissionsdf)
    if (resolution=='month'){
      emissionsdf$date <- month(emissionsdf$date)
    }
    #emissionsdf <- emissionsdf[emissionsdf$zone!='All Zones',]
    #emissionsdf$date <- as.Date(emissionsdf$datetime)
    #sum profit by day for plotting
    emissionsdf <- ddply(emissionsdf, ~ date + label, summarise, emissions = sum(Emissions))
    
    #print(storagedf)
    #geom_bar(stat='identity',
    ggplot(data=emissionsdf, aes(x=date, y=emissions, fill=label)) +
      geom_bar(stat='identity',position='dodge') + 
      theme_classic() + ylab("Emissions (tonne CO2)") + xlab("") +
      guides(fill=guide_legend(title="")) +
      theme(legend.text = element_text(size=32),
            legend.position = 'bottom',
            plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
            axis.title.y = element_text(size=28),
            axis.text.x= element_text(size=20),
            axis.text.y= element_text(size=20)) +
      ggtitle(paste("Total Emissions Comparison"))
    
    setwd(paste(baseWD, "post_processing", "figures", sep="/"))
    ggsave(paste0("total emissions plot",plotTitle,".png"), width=12, height=6)
    return(emissionsdf)
}

compareTotalGeneratorCost <- function(generatordflist,plotTitle='hi',resolution='NA'){
  #eventually probably a list of dfs as input
  for (i in 1:length(generatordflist)){
    generatordflist[[i]]$label <- names(generatordflist[i]) #may want better label
  }
  generatordf <- do.call("rbind", generatordflist)
  #print(generatordf)
  if (resolution=='month'){
    generatordf$date <- month(generatordf$date)
  }
  if (names(generatordf)[2]=="Revenue"){
    plotlabel <- names(generatordf)[2]
    generatordf <- ddply(generatordf, ~ date + label, summarise, cost = sum(Revenue))
  }
  else{
    plotlabel <- names(generatordf)[2]
    generatordf <- ddply(generatordf, ~ date + label, summarise, cost = sum(Cost))
  }
  
  ggplot(data=generatordf, aes(x=date, y=cost, fill=label)) +
    geom_bar(stat='identity',position='dodge') + 
    theme_classic() + ylab(paste("Generator ",plotlabel," ($)")) + xlab("") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=32),
          legend.position = 'bottom',
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=28),
          axis.title.x = element_blank(),
          axis.text.x= element_blank(),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("Total Generator ",plotlabel," Comparison"))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("total dispatch cost plot",plotTitle,".png"), width=12, height=6)
  return(generatordf)
}

#storage dispatch
plotStorage <- function(results, dates, plotTitle, hours=24){
  storage_dispatch <- results[["storage"]]
  storage_dispatch$dispatch <- storage_dispatch$discharge-storage_dispatch$charge
  storage_dispatch$X <- factor(storage_dispatch$X)
  storage_dispatch$node <- factor(storage_dispatch$node)

  #Luke's plotting code (active)
  plotcolors <- c("dispatch" = "black", "LMP" = "red")
  plotfill <- c("SOC" = "gray")
  
  ggplot(data=storage_dispatch, aes(x=datetime, y=soc,fill="SOC")) + geom_area(alpha=.5) + 
    facet_wrap(~X, nrow=1, scales = "free") +
    geom_line(aes(datetime, dispatch,color="dispatch"),lwd=3) +
    geom_line(aes(datetime, lmp,color="LMP"),lwd=2,linetype='dashed') +
    theme_classic() + ylab("MW or LMP ($/MWh)") + xlab("") +
    ylim(c(min(storage_dispatch$dispatch),max(storage_dispatch$soc))) +
    scale_x_datetime() +
    scale_color_manual(values = plotcolors) +
    scale_fill_manual(values=plotfill)+
    theme(legend.text = element_text(size=32),
          legend.title = element_blank(),
          legend.position = 'bottom',
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          strip.text.x = element_text(size=24),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("Storage Dispatch ", plotTitle))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("storage dispatch ", plotTitle, ".png"), width=20, height=12)
  
  return(storage_dispatch)
}

#compare storage dispatch
compareplotStorage <- function(storage_df1,storage_df2,plotTitle='NA'){

  storagedelta_df <- merge(storage_df1, storage_df2, by = c("X","datetime"))
  
  storagedelta_df$dispatch <- storagedelta_df$dispatch.x - storagedelta_df$dispatch.y
  storagedelta_df$soc <- storagedelta_df$soc.x - storagedelta_df$soc.y
  storagedelta_df$lmp <- storagedelta_df$lmp.x - storagedelta_df$lmp.y
  
  ggplot(data=storagedelta_df, aes(x=datetime, y=soc, fill="SOC")) + geom_area(alpha=0.5) + 
    facet_wrap(~X, nrow=1, scales = "free") +
    geom_line(aes(datetime, dispatch,color='Storage Dispatch'),lwd=3) +
    geom_line(aes(datetime, lmp,color='LMP'),lwd=2,linetype='dashed') +
    theme_classic() + ylab("MWh or LMP ($/MWh)") + xlab("") +
    scale_x_datetime() +
    scale_color_manual(name = "", values = c("Storage Dispatch" = "black", "LMP" = "red")) +
    scale_fill_manual(name="",values=c("SOC"="gray50")) +
    guides(color=guide_legend(nrow=1)) +
    theme(legend.text = element_text(size=32),
          legend.position = 'bottom',
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("Storage Delta Dispatch ", plotTitle))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("storage delta dispatch",".png"), width=20, height=12)
  
  }

compareStorageHeatplot <- function(storagedflist,plotTitle='NA',type='NA'){
  
  for (i in 1:length(storagedflist)){
    storagedflist[[i]]$label <- names(storagedflist[i]) #may want better label
  }
  storagedf <- do.call("rbind", storagedflist)
  
  #this isn't perfect but we have to reformat the hours based on datetime
  storagedf$hour <- format(as.POSIXct(ceiling_date(storagedf$datetime,unit = "hours"),format = "%Y-%m-%d %H:%M:%S"),"%H")
  storagedf$hour <- as.numeric(storagedf$hour)
  storagedf$hour[storagedf$hour==0] <- 24

  if (type=='lmp'){
    ggplot(data = storagedf, aes(x = hour, y = label, fill = lmp)) +
      facet_wrap(~X, nrow=2)+
      geom_tile() + geom_text(aes(label=round(lmp,0)))+
      theme_classic() +
      scale_fill_gradient2(low = "darkgreen",mid='yellow',high = "darkred",
                           na.value = "grey") +
      labs(fill="LMP ($/MWh)",x="Hour") +
      theme(legend.position="right",
            text = element_text(size=14),
            legend.text=element_text(size=28),
            legend.key.size = unit(3,"line"),
            axis.text.x = element_text(size=24),
            axis.text.y = element_text(size=24),
            axis.title.y = element_text(size=28),
            axis.title.x = element_text(size=28))+
      ggtitle(paste("Storage LMP Heatplot ", plotTitle))
  }
  else{
    ggplot(data = storagedf, aes(x = hour, y = label, fill = dispatch)) +
      facet_wrap(~X, nrow=2)+
      geom_tile() + geom_text(aes(label=round(dispatch,-1)))+
      theme_classic() +
      scale_fill_gradient2(low = "darkred",mid="white",high = "darkgreen",
                           midpoint=0,na.value = "grey") +
      labs(fill="Avg Dispatch \n (MW)",x="Hour") +
      theme(legend.position="right",
            text = element_text(size=16),
            legend.text=element_text(size=28),
            legend.key.size = unit(3,"line"),
            axis.text.x = element_text(size=24),
            axis.text.y = element_text(size=24),
            axis.title.y = element_text(size=28),
            axis.title.x = element_text(size=28))+
      ggtitle(paste("Storage Dispatch Heatplot ", plotTitle))
  }
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("storage heatplot ",plotTitle,".png"), width=16, height=6)
}


compareStorageProfit <- function(storagedflist,plotTitle='hi',resolution='NA'){
  
  
  #ss <- results1$storageresources$Storage_Index[results1$storageresources$StorageIndex == 1]
  #eventually probably a list of dfs as input
  for (i in 1:length(storagedflist)){
    #storagedflist[[i]] <- storagedflist[[i]][storagedflist[[i]]$X==ss,]
    storagedflist[[i]]$label <- names(storagedflist[i]) #may want better label
  }
  storagedf <- do.call("rbind", storagedflist)
  
  #storagedf <- storagedf[storagedf$profit>=0,]
  
  if (resolution=='month'){
    storagedf$date <- month(storagedf$date)
  }
  #print(storagedf)
  #sum profit by day for plotting
  storagedf <- ddply(storagedf, ~ date + label, summarise, storageprofit = sum(profit))
  storagedf$capacity <- max(storagedflist[[1]]$discharge)
  storagedf$duration <- max(storagedflist[[1]]$soc)
  
  #print(storagedf)
  #geom_bar(stat='identity',
  ggplot(data=storagedf, aes(x=date, y=storageprofit, fill=label)) +
    geom_bar(stat='identity',position='dodge') + 
    theme_classic() + ylab("Profit($)") + xlab("") +
    theme(legend.text = element_text(size=26),
          legend.title = element_blank(),
          legend.position = 'bottom',
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_blank(),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("Storage Profit ", plotTitle))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("storage profit plot",plotTitle,".png"), width=12, height=6)
  return(storagedf)
}

cleanOffer <- function(results,dates,hours=24){
  offer <- results[['offer']]
  gens <- results[['gens']]
  
  offer$segID <- gsub("[[:print:]]*-", "", offer[,1])
  offer$genID <- gsub("-\\d+","",offer[,1])
  offer$area <- substr(offer$Zone,start=1,stop=1)
  
  offer <- merge(offer, gens[,c("Name", "Category")], by.x="genID", by.y="Name", all.x=T)
  #drop duplicated entries in output
  offer <- offer[!duplicated(offer), ]
  #print(offer)
  profits <- ddply(offer, ~ date + area, summarise, Profits = sum(Profit))
  return(profits)
}

compareGeneratorProfit <- function(generatordflist,plotTitle='hi',resolution='NA'){
  #eventually probably a list of dfs as input
  #print(names(generatordflist))
  for (i in 1:length(generatordflist)){
    generatordflist[[i]]$label <- names(generatordflist[i]) #may want better label
  }
  gendf <- do.call("rbind", generatordflist)
  
  if (resolution=='month'){
    gendf$date <- month(gendf$date)
  }
  
  gendf <- ddply(gendf, ~ date + label, summarise, profit = sum(Profit))
  
  ggplot(data=gendf, aes(x=date, y=profit, fill=label)) +
    geom_bar(stat='identity',position='dodge') + 
    theme_classic() + ylab("Profit($)") + xlab("") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=32),
          legend.position = 'bottom',
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_blank(),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("Generator Profit ", plotTitle))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("generator profit plot",plotTitle,".png"), width=12, height=6)
  return(gendf)
}

#cleanOffer(results1,dates1) #takes awhile bc big file

cleanEmissions <- function(results,dates,hours=24){
  offer <- results[['offer']]
  #gens <- results[['gens']]
  
  offer$segID <- gsub("[[:print:]]*-", "", offer[,1])
  offer$genID <- gsub("-\\d+","",offer[,1])
  offer$area <- substr(offer$Zone,start=1,stop=1)

  emissions <- ddply(offer,~date,summarise,Emissions=sum(SegmentEmissions))
  return(emissions)
}

cleanDispatchCost <- function(results,dates,type='NA',filter='None',hour=24){
  offer <- results[['offer']]
  #print(offer)
  offer$segID <- gsub("[[:print:]]*-", "", offer[,1])
  offer$genID <- gsub("-\\d+","",offer[,1])
  offer$area <- substr(offer$Zone,start=1,stop=1)
  if (type=='lmp'){
    offer$dispatchcost <- offer$SegmentDispatch*offer$LMP
  }
  else{
    offer$dispatchcost <- offer$SegmentDispatch*offer$MarginalCost
  }
  #print(offer)
  if (filter!='None'){
    offer <- offer[offer$genID==filter,]
  }
  DispatchCost <- ddply(offer,~date,summarise,Cost=sum(dispatchcost))
  if (type=="lmp"){
    names(DispatchCost) <- c("date","Revenue")
  }
  return(DispatchCost)
}


cleanDispatchProfit <- function(results,dates,type='NA',filter='None',overwrite_gen="None",hour=24){
  gc <- results[['generatorresources']]$Gen_Index[results[['generatorresources']]$GencoIndex == 1]
  offer <- results[['offer']]
  offer <- offer[,c("X","SegmentDispatch","LMP","date")]
  
  nucoffer <- results[['nucoffer']]
  nucoffer <- nucoffer[,c("X","dispatch","lmp","date")]
  colnames(nucoffer)=c("X","SegmentDispatch","LMP","date")
  offer <- rbind(offer, nucoffer)
  offer$segID <- gsub("[[:print:]]*-", "", offer[,1])
  offer$genID <- gsub("-\\d+","",offer[,1])
  if (overwrite_gen=="None"){
    offer <- offer[offer$genID==gc,]
  }
  else{
    offer <- offer[offer$genID==overwrite_gen,]
  }
  offer$dispatchprofit <- offer$SegmentDispatch * offer$LMP
  if (filter!='None'){
    offer <- offer[offer$genID==filter,]
  }
  DispatchProfit <- ddply(offer,~date,summarise,Profit=sum(dispatchprofit))
  return(DispatchProfit)
}

compareObjectives <- function(resultslist){
  objectivelist <- list()
  for (i in 1:length(resultslist)){
    resultslist[[i]]$objective$label <- names(resultslist[i])
    #objectiveframe$label <- names(resultslist[i])
    #objectivelist <- c(objectivelist, objectiveframe)
  }
  objectivedf <- rbind(resultslist[[2]]$objective,resultslist[[1]]$objective)
  print(objectivedf)
  valuedf <- ddply(objectivedf, ~ date + label, summarise, Objective = sum(GeneratorProfitDual))
  gapdf <- ddply(objectivedf, ~ date + label,summarise, Gap = sum(gap))
  profitdf <- ddply(objectivedf, ~ date + label, summarise, Objective = sum(SSProfit))
  valuedf$Gap <- gapdf$Gap
  valuedf$SSProfit <- profitdf$Objective
  print(valuedf)
  
  ggplot(data=valuedf, aes(x=date, y=Objective, fill=label)) +
    geom_bar(stat='identity',position='dodge') +
    geom_errorbar(aes(x=date, ymin=Objective-Gap, ymax=Objective+Gap), colour="black", position='dodge')+
    theme_classic() + ylab("Objective($)") + xlab("") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=32),
          legend.position = 'bottom',
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("Daily Objectives"))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("dailyobjectives",".png"), width=12, height=6)
  return(valuedf)
  
}

plotAllPrices <- function(pricelist,caselabel,busIDlist=list("03","03")){
  
  for (i in 1:length(pricelist)){
    pricelist[[i]]$label <- names(pricelist[i]) #may want better label
    pricelist[[i]] <- pricelist[[i]][pricelist[[i]]$busID %in% c(busIDlist[[i]]),]
  }
  pricedf <- do.call("rbind", pricelist)
  
  if (length(pricelist)==3){
    plotcolors <- c(A="red",B="black",C="blue")
    plotlines <- c("solid","solid","solid")
    labs <- c("STRATEGIC CONGESTED","COMPETITIVE CONGESTED","UNCONGESTED")
  }
  else if (length(pricelist)==2){
    plotcolors <- c(A="red",B="black")
    plotlines <- c("solid","solid")
    labs <- c("STRATEGIC","COMPETITIVE")
  }
  else{
    print('length is not 2 or 3, so no plot for you')
    return(pricedf)
  }
  
  
  
  #Luke's plotting code (active)
  ggplot(data=pricedf, aes(x=datetime, y=LMP, color=label)) + geom_line(lwd=2.5) + 
    theme_classic() + ylab("$/MWh") + xlab("") +
    scale_x_datetime() +
    scale_color_manual(values = plotcolors, labels=labs) +
    scale_linetype_manual(values=plotlines) +
    guides(colour=guide_legend(title="", nrow=1))+
    theme(legend.text = element_text(size=28),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=40),
          axis.text.x= element_text(size=32),
          axis.text.y= element_text(size=32),
          strip.text.x = element_text(size = 24))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("allprices",caselabel,".png"),dpi=500, width=16, height=4)
  return(pricedf)
}

dates3 <- seq(as.POSIXct("1/15/2019", format = "%m/%d/%Y"), by="day", length.out=7)
resultsSS300_900_dates3 <-loadResults(dates3,folder='303SS_300_900',subfolder="results_DA")
resultsNSS300_900_dates3 <- loadResults(dates3,folder='303NSS_300_900',subfolder="results_DA")

pricesSS <- plotPrices(resultsSS300_900_dates3,dates3,plotTitle='SSJan')
pricesNSS <- plotPrices(resultsNSS300_900_dates3,dates3,plotTitle='NSSJan')
plist <- list(pricesSS,pricesNSS)
names(plist) <- c("A","B")
f<-plotAllPrices(plist,"ESRonly303")

dates3 <- seq(as.POSIXct("1/15/2019", format = "%m/%d/%Y"), by="day", length.out=5)
resultsWINDSS300_900_dates3 <-loadResults(dates3,folder='303SS_Wind303_300_900',subfolder="results_DA")
resultsWINDNSS300_900_dates3 <- loadResults(dates3,folder='303NSS_Wind303_300_900',subfolder="results_DA")
resultsWINDSS300_900_MITIGATE <- loadResults(dates3,folder="303SS_Wind303_300_900",subfolder = "results_DA_Mitigated")

pricesWINDSS <- plotPrices(resultsWINDSS300_900_dates3,dates3,plotTitle='SSJan')
pricesWINDNSS <- plotPrices(resultsWINDNSS300_900_dates3,dates3,plotTitle='NSSJan')
pricesWINDSSMITIGATE <- plotPrices(resultsWINDSS300_900_MITIGATE,dates3,plotTitle="mitigateJan")
plistWIND <- list(pricesWINDSS,pricesWINDNSS,pricesWINDNSS)
names(plistWIND) <- c("A","B","C")
f2<-plotAllPrices(plistWIND,"ESRandWind303",busIDlist = list("03","03","13"))

compareForecastErrors <- function(df1,df2){
  dispatchdelta_df <- merge(df1, df2, by = c("datetime","Category","zone"))
  dispatchdelta_df$MW <- dispatchdelta_df$MW.x - dispatchdelta_df$MW.y
  
  dispatchdelta_df <- dispatchdelta_df[dispatchdelta_df$zone=="All Zones",]
  dispatchdelta_df$hour <- hour(dispatchdelta_df$datetime)
  dispatchdelta_df$day <- day(dispatchdelta_df$datetime)
  totals <- dispatchdelta_df %>% group_by(Category,hour,day) %>% summarise(TotalMW=mean(MW))
  dispatchdelta_df <- dispatchdelta_df %>% group_by(Category,hour,day) %>% summarise(MW=mean(MW))
  dispatchdelta_df$totals <- totals$TotalMW  #do also a totals col so can get %
  
  
  Wind <- dispatchdelta_df[dispatchdelta_df$Category=="Wind",]
  Solar <- dispatchdelta_df[dispatchdelta_df$Category=="Solar PV",]
  RTPV <- dispatchdelta_df[dispatchdelta_df$Category=="Solar RTPV",]
  Load <- dispatchdelta_df %>% group_by(hour,day) %>% summarise(MW=sum(MW),totals=sum(totals))
  Load$Category <- "Load"
  
  FE_df <- rbind(Wind,Solar,RTPV,Load)
  print(FE_df)
  
  ggplot(data=FE_df, aes(x=Category, y=MW, fill=Category)) +
    geom_boxplot() +
    theme_classic() + ylab("MW") + xlab("") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=32),
          legend.position = 'none',
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("Forecast Error Distributions"))
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("forecastErrors",".png"), dpi=500,width=12, height=6)
}

compareForecastErrors2 <- function(rlist){
  #rlist$generatoravail[grep("WIND",rlist$generatoravail$Gen_Index), ]
  LoadDF <- merge(rlist$zonalLoad, rlist$zonalLoadRT, by = c("timepoint","zone","date"))
  GenDF <- merge(rlist$generatoravail,rlist$generatoravailrt,by=c("timepoint","Gen_Index","date"))
  LoadDF$gross_load <- LoadDF$gross_load.x - LoadDF$gross_load.y
  #LoadDF$pct <- LoadDF$gross_load/LoadDF$gross_load.y
  
  GenDF$Capacity <- GenDF$Capacity.x-GenDF$Capacity.y
  #GenDF$pct <- GenDF$Capacity/GenDF$Capacity.y

  LoadDF <- LoadDF %>% group_by(date,timepoint) %>% summarise(MW=sum(gross_load))
  LoadDF$label <- "Load"
  
  Wind <- GenDF[grep("WIND",GenDF$Gen_Index), ]
  Wind <- Wind %>% group_by(date,timepoint) %>% summarise(MW=sum(Capacity))
  Wind$label <- "Wind"
  Solar <- GenDF[grep("_PV_",GenDF$Gen_Index), ]
  Solar <- Solar %>% group_by(date,timepoint) %>% summarise(MW=sum(Capacity))
  Solar$label <- "PV"
  RTPV <- GenDF[grep("RTPV",GenDF$Gen_Index), ]
  RTPV <- RTPV %>% group_by(date,timepoint) %>% summarise(MW=sum(Capacity))
  RTPV$label <- "RTPV"
  
  #create a net load FE
  NetLoadDF <- LoadDF
  NetLoadDF$label <- "Net Load"
  
  NetLoadDF$MW <- LoadDF$MW-Wind$MW-Solar$MW-RTPV$MW
  
  #group by tmp and date
  FE_df <- rbind(Wind,Solar,RTPV,LoadDF,NetLoadDF)
  
  ggplot(data=FE_df, aes(x=label, y=MW, fill=label)) +
    geom_boxplot() +
    theme_classic() + ylab("MW (DA-RT)") + xlab("") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=32),
          legend.position = 'none',
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("Forecast Error Distributions"))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("forecastErrors",".png"), dpi=500,width=12, height=6)
  return(FE_df)
}

LoadWindScatter <- function(rlist,rlistcompared){
  #grab thermal gen realized
  offer <- rlist$offer
  marginalgens <- offer[(offer$SegmentDispatch!=0 & offer$AvailableSegmentCapacity-offer$SegmentDispatch>0),]
  #print(marginalgens)
  offer <- offer[!grepl("HYDRO",offer$X),]
  offer <- offer %>% group_by(date,hour) %>% summarise(MW=sum(SegmentDispatch))
  #print(offer)
  
  #total curtailment
  rlist$nucoffer$curtailed <- rlist$nucoffer$capacity-rlist$nucoffer$dispatch
  curtailed_df <- rlist$nucoffer%>%group_by(date,hour)%>%summarise(curtailMW=sum(curtailed))
  #print(curtailed_df)  
  #grab wind 303
  Wind303<-rlist$nucoffer[grep("303_WIND",rlist$nucoffer$X),]
  Wind303$curtailed <- Wind303$capacity-Wind303$dispatch
  #combine
  offer$Wind <- Wind303$dispatch
  offer$CurtailedWind <- Wind303$curtailed
  offer$LMP <- Wind303$lmp
  
  #then the strategic prices
  for (i in 1:length(rlistcompared)){
    W <- rlistcompared[[i]]$nucoffer[grep("303_WIND",rlistcompared[[i]]$nucoffer$X),]
    colname <- paste0("compareLMP",i)
    deltaname <- paste0("deltaLMP",i)
    offer[,colname] <- W$lmp
    offer[,deltaname] <- offer[,colname]-offer$LMP
  }
  #Wind303SS <- rlistcompared$nucoffer[grep("303_WIND",rlistcompared$nucoffer$X),]
  #offer$compareLMP <- Wind303SS$lmp
  #offer$deltaLMP <- offer$compareLMP-offer$LMP
  vals <- offer[offer$deltaLMP1>5,]
  
  #merge the realized storage dispatch in these hours
  rlist$storage$hour <- hour(rlist$storage$datetime)+1
  rlist$storage$dispatch <- rlist$storage$discharge -rlist$storage$charge
  competitivestorage <- rlist$storage %>% group_by(date,hour) %>% summarise(Competitive=mean(dispatch)) 
  vals <- merge(vals, competitivestorage, by = c("date","hour"))
  
  #then the strategic
  rlistcompared[[1]]$storage$hour <- hour(rlistcompared[[1]]$storage$datetime)+1
  rlistcompared[[1]]$storage$dispatch <- rlistcompared[[1]]$storage$discharge -rlistcompared[[1]]$storage$charge
  strategicstorage <- rlistcompared[[1]]$storage %>% group_by(date,hour) %>% summarise(Strategic=mean(dispatch),
                                                                                       strategicdischargeoffer=mean(discharge_offer),
                                                                                       strategicchargeoffer=mean(charge_offer)) 
  vals <- merge(vals, strategicstorage, by = c("date","hour"))
  vals$offer <- ifelse(as.numeric(as.character(vals$Strategic))>0,as.numeric(vals$strategicdischargeoffer),as.numeric(vals$dischargeoffer))
  vals$offer <- ifelse(vals$offer<0,-1,vals$offer)
  vals$offer <- ifelse(is.na(vals$offer),100,vals$offer)
  #also handle the NAs
  
  #add something also for hourly total load
  #TotalMW <- resultsD$zonalLoad%>%group_by(date,timepoint)%>%summarise(TotalMW=sum(gross_load))
  #offer$totalMW <- TotalMW$TotalMW
  #geom_point(data=offer[offer$deltaLMP2>5,],aes(x=MW,y=Wind),fill="orange",color="black",shape=22,size=7,inherit.aes=F) +
  #geom_point(data=offer[offer$deltaLMP3>5,],aes(x=MW,y=Wind),fill="purple",color="black",shape=22,size=5,inherit.aes=F) +
  #geom_point(data=offer[offer$deltaLMP<-1,],aes(x=MW,y=Wind),fill="green",color="black",shape=23,size=5,inherit.aes=F)+
  #then try a scatter
  ggplot(data=offer[offer$deltaLMP1>5,],aes(x=MW,y=Wind)) +
    geom_point(color="black",fill="blue",shape=22,size=12) +
    geom_point(data=offer, aes(x=MW, y=Wind,fill=LMP),color="black",shape=21,size=5,inherit.aes=F)+
    scale_fill_gradient2(low="white",high="red")+
    theme_classic() + ylab("Bus 303 DA Wind\nGeneration (MW)") + xlab("Cleared Thermal MW \n (>0 when 303 LMP=0 indicates congestion)") +
    guides(fill=guide_legend(title="Competitive Bus\n303 LMP ($/MWh)")) +
    annotate("rect", xmin=1000, xmax=1050, ymin=665 , ymax=735, color="black", fill="blue") +
    annotate("text", label = "Leveraged by \n Strategic Storage", size = 8, x = 1250, y = 700) +
    theme(legend.title = element_text(size=24),
          legend.text = element_text(size=24),
          legend.position = 'bottom',
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20))+
    ggtitle(paste("January Hourly Scatter of Gen and LMPs"))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("LMPScatter300900",".png"), dpi=500,width=12, height=6)
  
  
  
  #order from greatest to least for plot
  vals <- vals[order(-vals$deltaLMP1),]
  #melt to make df tidy
  vals <- vals[,c("date","hour","deltaLMP1",'Strategic','Competitive',"offer")]
  vals$type <- ifelse(vals$Competitive-vals$Strategic>1,"Physical","Economic")
  vals$type <- ifelse(vals$type=="Physical" & vals$offer>0 & vals$offer<100, "Congestion",vals$type)
  vals <- melt(vals, id.vars=c("date","hour","deltaLMP1","offer","type"))
  
  
  vals$deltaLMP1 <- round(vals$deltaLMP1, digits = 1)
  vals$deltaLMP1 <- str_pad(vals$deltaLMP1, 4, pad='0')
  #vals$deltaLMP1 <- sprintf("%02f", vals$deltaLMP1)
  #vals$delatLMP1 <- as.factor(as.character(vals$deltaLMP1))
  vals$deltaLMP1 <- as.factor(paste0("????=$",vals$deltaLMP1,"/MWh"))
  
  
  
  vals$mylabel <- paste0(vals$date," HE",vals$hour)
  vals$mylabel <- gsub("2019-","",vals$mylabel)
  vals$leveler <- paste0(vals$deltaLMP1,vals$mylabel)
  
  vals$mylabel <- factor(vals$mylabel,levels=unique(vals$mylabel[order(vals$leveler)]))
  
  

  vals$offer <- round(vals$offer,digits=1)
  vals$offer <- paste0("Offer=$",vals$offer,"/MWh, ",vals$type," Withholding")
  #vals$offer[vals$variable="Competitive"] <- " "
  
  
  #plot
  ggplot(vals, aes(x = mylabel, y = value, fill = variable)) +
    geom_bar(stat = "identity",position="dodge") +
    coord_flip() + ylim(c(-200,540))+
    geom_text(data=vals[vals$variable=="Strategic",],aes(x=mylabel,y=430,label=offer),size=5)+
    geom_text(data=vals[vals$variable=="Strategic",],aes(x=mylabel,y=-175,label=deltaLMP1),size=5)+
    xlab("Month-Day Hour Ending") +
    ylab("ESR Dispatch (MW)")+
    theme_classic() +
    guides(fill=guide_legend(title=""))+
    theme(legend.title = element_text(size=24),
          legend.text = element_text(size=24),
          legend.position = 'bottom',
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=12))
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("BidDispatchbars",".png"), dpi=500,width=12, height=8)
  
  
  return(vals)
}

profitResultsPairs <- function(pairlist, dates, labels=c("Strategic","Competitive")){
  objectivelist <- c()
  for (i in 1:length(pairlist)){
    strategic <- pairlist[[i]][[1]]
    
    nonstrategic <- pairlist[[i]][[2]]
    
    #first grab objective-related values
    strategic$objective$label <- labels[[1]]
    nonstrategic$objective$label <- labels[[2]]
    objectivedf <- rbind(strategic$objective,nonstrategic$objective)
    gapdf <- ddply(objectivedf, ~ label,summarise, Gap = sum(gap))
    #print(gapdf)
    objectivelist <- c(objectivelist,gapdf$Gap)

    
    clist <- list(cleanDispatchProfit(strategic,dates,overwrite_gen="303_WIND_1"), cleanDispatchProfit(nonstrategic,dates,overwrite_gen="303_WIND_1"))
    slist <- list(plotStorage(strategic,dates,plotTitle='1'),
                  plotStorage(nonstrategic,dates,plotTitle='1'))
    
    names(clist) <- labels
    names(slist) <- labels
    print('yo')
    #print(clist)
    totalprofit <- compareGeneratorProfit(clist,plotTitle=" ",resolution="month")
  
    storageprofit <- compareStorageProfit(slist,plotTitle=" ",resolution="month")
    print('progress')
    #do a merge to get total profit
    totalprofit$storageprofit <- storageprofit$storageprofit
    totalprofit$storagecapacity <- storageprofit$capacity
    totalprofit$storageduration <- storageprofit$duration
    totalprofit$totalprofit <- totalprofit$profit+totalprofit$storageprofit
    totalprofit$totalprofit <- totalprofit$totalprofit - rep(totalprofit$profit[1],2)
    totalprofit$deltaprofit <- c(totalprofit$storageprofit[1],totalprofit$storageprofit[2]+totalprofit$profit[2]-totalprofit$profit[1])
    if (i==1){
      finaldf <- totalprofit
    }
    else{
      finaldf <- rbind(finaldf,totalprofit)
    }
    print(finaldf)
  }

  #eventually format dataframe
  #profitdf <- do.call("rbind", caselist)
  finaldf$hours <- as.character(finaldf$storageduration/finaldf$storagecapacity)
  finaldf$perMwhprofit <- finaldf$deltaprofit/(finaldf$storageduration*31)
  #$perMWhprofit <- finaldf$perMWhprofit/31 #for now but change 
  finaldf$storagecapacity <- as.numeric(finaldf$storagecapacity)
  finaldf$hourlabel <- paste(finaldf$hour,finaldf$label,sep="_")
  
  #geom_line(aes(group = hourlabel, linetype=label))+
  #scale_linetype_manual(values=c("solid","dashed"))+
  #then plotting
  ggplot(data=finaldf,aes(x=perMwhprofit,y=totalprofit/1000,color=hours)) +
    geom_point(aes(size=storagecapacity,shape=label)) +
    scale_size_continuous(range = c(3, 12))+
    theme_classic() + ylab("Monthly Incremental ESR Profit ($K)") + xlab("Monthly Incremental ESR Profit ($/MWh dispatched)") +
    guides(color=guide_legend(title="ESR Duration (h)"),
           size=guide_legend(title="ESR Capacity (MW)"),
           shape=guide_legend(title="ESR Bidding")) +
    theme(legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.position = 'right',
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("profitscatter",".png"), dpi=500,width=12, height=6)
  
  #try also a grouped stacked barplot
  melted <- finaldf[,c("label","profit","storageprofit")]
  colnames(melted) <- c("label","Wind","ESR")
  
  melted$cat <- c("Case A: ESR Only","Case A: ESR Only","Case B: ESR+Wind","Case B: ESR+Wind")
  #melted$cat <- c("Case B: RT Perfect Foresight","Case B: RT Perfect Foresight","Case B: RT Uncertainty","Case B: RT Uncertainty")
  
  melted <- melt(melted, id.vars=c("label","cat"))
  melted$label <- ifelse(melted$label=="Competitive","Competitive","Strategic ESR Bids")
  
  melted$Gap <- objectivelist
  melted$Gap <- melted$Gap*.001
  melted$value <- melted$value*.001
  
  melted$Gap[melted$label=="Competitive"] <- 0#competitive is LP so ovewrite gap as 0
  #adjust positioning
  grouped_melt <- melted %>% group_by(cat,label) %>% summarise(totalvalue=sum(value))
  melted$totalvalue <- rep(grouped_melt$totalvalue,2)
  print(melted)
  
  ggplot(melted, aes(x = label, y = value, fill = variable)) +
    geom_bar(stat = 'identity', position = 'stack',alpha=.7) +
    geom_errorbar(aes(x=label, ymin=totalvalue-Gap, ymax=totalvalue+Gap),position='identity', colour="black")+
    facet_grid(~ cat) + theme_classic()+ ylab("Monthly Profit ($K)") +
    xlab("") + scale_fill_manual(values=c("cyan","grey"))+
    guides(fill=guide_legend(title="Generator Profit")) +
    theme(legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.position = 'bottom',
          plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=16),
          axis.text.y= element_text(size=20),
          strip.text.x = element_text(size=24))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("profitstackbar",".png"), dpi=500,width=10, height=6)
  
  return(finaldf)

}
profitResultsPairs(list(list(resultsSS300_900,resultsNSS300_900),
                        list(resultsWINDSS300_900,resultsNSS300_900)),
                   dates1)

a<-LoadWindScatter(resultsD,list(resultsC,resultsC,resultsF))
e<-LoadWindScatter(resultsA,resultsB)

dates1 <- seq(as.POSIXct("1/1/2019", format = "%m/%d/%Y"), by="day", length.out=31) # Configure cases period here
dates2 <- seq(as.POSIXct("1/1/2019", format = "%m/%d/%Y"), by="day", length.out=59)
dates3 <- seq(as.POSIXct("1/15/2019", format = "%m/%d/%Y"), by="day", length.out=5)
resultsDispatch <- loadResults(dates3,folder='NoStorageDispatch',subfolder="results_DA")
disp<-plotDispatch(resultsDispatch,dates3,plotTitle='JanFeb 2019 dispatch')
plotPrices(resultsDispatch,dates3,plotTitle='January prices')

results1 <- loadResults(dates1,folder='NoStorageDispatch',subfolder="results_DA")
results2 <- loadResults(dates3,folder='NoStorageDispatch',subfolder="results_DA_RTVRE")

### profit scatter
resultsSS50_150 <-loadResults(dates1,folder='303SS_Wind303_50_150',subfolder="results_DA")
resultsNSS50_150 <- loadResults(dates1,folder='303NSS_Wind303_50_150',subfolder="results_DA")
resultsSS100_300 <-loadResults(dates1,folder='303SS_Wind303_100_300',subfolder="results_DA")
resultsNSS100_300 <- loadResults(dates1,folder='303NSS_Wind303_100_300',subfolder="results_DA")
resultsSS200_600 <-loadResults(dates1,folder='303SS_Wind303_200_600',subfolder="results_DA")
resultsNSS200_600 <- loadResults(dates1,folder='303NSS_Wind303_200_600',subfolder="results_DA")
resultsSS300_900 <-loadResults(dates1,folder='303SS_Wind303_300_900',subfolder="results_DA")
resultsNSS300_900 <- loadResults(dates1,folder='303NSS_Wind303_300_900',subfolder="results_DA")
resultsSS500_1500 <-loadResults(dates1,folder='303SS_Wind303_300_900',subfolder="results_DA")
resultsNSS500_1500 <- loadResults(dates1,folder='303NSS_Wind303_300_900',subfolder="results_DA")

resultsSS50_50 <-loadResults(dates1,folder='303SS_Wind303_50_50',subfolder="results_DA")
resultsNSS50_50 <- loadResults(dates1,folder='303NSS_Wind303_50_50',subfolder="results_DA")
#resultsSS300_300 <-loadResults(dates1,folder='303SS_Wind303_300_300',subfolder="results_DA")
#resultsNSS300_300 <- loadResults(dates1,folder='303NSS_Wind303_300_300',subfolder="results_DA")

resultsSS300_2400 <-loadResults(dates1,folder='303SS_Wind303_300_2400',subfolder="results_DA")
resultsNSS300_2400 <- loadResults(dates1,folder='303NSS_Wind303_300_2400',subfolder="results_DA")

profitResultsPairs(list(list(resultsSS50_150,resultsNSS50_150),
                        list(resultsSS100_300,resultsNSS100_300),
                        list(resultsSS200_600,resultsNSS200_600),
                        list(resultsSS300_900,resultsNSS300_900),
                        list(resultsSS50_50,resultsNSS50_50)),dates1)
# end profit scatter
#checker 
clist <- list(cleanDispatchProfit(resultsNSS50_150,dates1),
              cleanDispatchProfit(resultsSS50_150,dates1))
names(clist) <- c("NSS","SS")
totalprofit <- compareGeneratorProfit(clist,plotTitle=" ",resolution="")

#end checker

resultsSS300_900_DABIND <- loadResults(dates1,folder='303SS_Wind303_300_900',subfolder="results_Bind_DA")
resultsNSS300_900_RTVRE <- loadResults(dates1,folder='303NSS_Wind303_300_900',subfolder="results_DA_RTVRE")
resultsSS300_900_RTVRE <- loadResults(dates1,folder='303SS_Wind303_300_900',subfolder="results_DA_RTVRE")

slist <- list(plotStorage(resultsNSS300_900_RTVRE,dates1,plotTitle='1'),
              plotStorage(resultsSS300_900_DABIND,dates1,plotTitle='1'),
              plotStorage(resultsSS300_900_RTVRE,dates1,plotTitle="1"))
names(slist) <- c("RTNSS","dabindRT","RToptimal")
esrprofits <- compareStorageProfit(slist,plotTitle=" ",resolution="")


clist <- list(cleanDispatchProfit(resultsNSS300_900_RTVRE,dates1),
              cleanDispatchProfit(resultsSS300_900_DABIND,dates1),
              cleanDispatchProfit(resultsSS300_900_RTVRE,dates1))
names(clist) <- c("RTNSS","dabindRT","RToptimal")
totalprofit <- compareGeneratorProfit(clist,plotTitle=" ",resolution="")

profitResultsPairs(list(list(resultsSS300_900_RTVRE,resultsNSS300_900_RTVRE),
                        list(resultsSS300_900_DABIND,resultsNSS300_900_RTVRE)),
                    dates1)


mean(resultsSS300_900$nucoffer$capacity[grepl("303_WIND",resultsSS300_900$nucoffer$X)]) #calculates wind avg gen

resultsSS300_900 <-loadResults(dates1,folder='303SS_300_900',subfolder="results_DA")
resultsWINDSS300_900 <-loadResults(dates1,folder='303SS_Wind303_300_900',subfolder="results_DA")
resultsNSS300_900 <- loadResults(dates1,folder='303NSS_Wind303_300_900',subfolder="results_DA")
resultsWINDSS300_900_MITIGATE <- loadResults(dates3,folder="303SS_Wind303_300_900",subfolder = "results_DA_Mitigated")

LoadWindScatter(resultsNSS300_900,list(resultsWINDSS300_900))
profitResultsPairs(list(list(resultsWINDSS300_900,resultsNSS300_900),
                        list(resultsSS300_900,resultsNSS300_900)),
                   dates1)
slist <- list(plotStorage(resultsSS300_900,dates1,plotTitle='1'),
              plotStorage(resultsWINDSS300_900,dates1,plotTitle='1'))
names(slist) <- c("ESRonly","windtoo")
esrprofits <- compareStorageProfit(slist,plotTitle=" ",resolution="")

clist <- list(cleanDispatchProfit(resultsSS300_900,dates1, overwrite_gen="303_WIND_1"),
              cleanDispatchProfit(resultsWINDSS300_900,dates1))
names(clist) <- c("ESRonly","windtoo")
totalprofit <- compareGeneratorProfit(clist,plotTitle=" ",resolution="")




#load/gen payment calcs
caselist <- list(cleanDispatchCost(resultsSS300_900,dates1,type='lmp'),
                 cleanDispatchCost(resultsWINDSS300_900,dates1,type='lmp'),
                 cleanDispatchCost(resultsNSS300_900,dates1,type="lmp"))
names(caselist) <- c("SS","SSWIND","NSS")
compareTotalGeneratorCost(caselist,plotTitle='JanPmt',resolution='month')

#storage
SS <- plotStorage(resultsSS300_900,plotTitle="SS")
SS <- SS %>% group_by(X,hour,date) %>% summarise(profit=sum(profit))
sum(SS$profit[SS$profit>0])
sum(SS$profit[SS$profit<0])

NSS <- plotStorage(resultsNSS300_900,plotTitle="NSS")
NSS <- NSS %>% group_by(X,hour,date) %>% summarise(profit=sum(profit))
sum(NSS$profit[NSS$profit>0])
sum(NSS$profit[NSS$profit<0])
