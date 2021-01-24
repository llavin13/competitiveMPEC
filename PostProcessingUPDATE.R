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


baseWD <- "C:/Users/llavin/Desktop/competitiveMPEC12_8"


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
  storageresources <- readFiles("storage_resources.csv", dates, dateResultsWD, subFolder="inputs")
  generatorresources <- readFiles("generators.csv", dates, dateResultsWD, subFolder="inputs")
  #emissions <- readFiles("generator_segment_marginalcost.csv", dates, dateResultsWD, subFolder="inputs")
  
  #clean and format
  modelLMP <- AddDatetime(modelLMP)
  storage <- AddDatetime(storage)
  dispatch <- AddDatetime(dispatch)
  offer <- AddDatetime(offer)
  nucoffer <- AddDatetime(nucoffer)
  
  # return results
  results <- list(modelLMP, zonalLoad, dispatch, gens, txFlows, storage, offer, nucoffer, storageresources, generatorresources, objective)
  names(results) <- c("modelLMP", "zonalLoad", "dispatch", "gens", "txFlows", "storage", "offer", "nucoffer", "storageresources", "generatorresources", "objective")
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

plotPrices <- function(results,dates,plotTitle,hours=24,busfilter="NA"){
  prices <- results[['modelLMP']]
  prices$zone <- substr(prices[,1],start=1,stop=1)
  prices$zone <- paste0("Area ",prices$zone)
  prices$busID <- substr(prices[,1],start=2,stop=3)
  prices$X <- as.character(prices$X)
  
  if (busfilter!="NA"){
    prices <- prices[prices$X %in% c(busfilter),]
  }
  
  #print(prices)
  #Luke's plotting code (active)
  ggplot(data=prices, aes(x=datetime, y=LMP, color=busID)) + geom_line(lwd=1.5) +
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
    ggtitle(paste("LMP by Bus for", plotTitle))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("prices ", plotTitle, ".png"), width=20, height=8)
  
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

plotAllPrices <- function(pricelist,generatordflist,LMP=T){
  
  for (i in 1:length(pricelist)){
    pricelist[[i]]$label <- names(pricelist[i]) #may want better label
  }
  pricedf <- do.call("rbind", pricelist)
  plotcolors <- c(C="red",D="black",E="blue")
  plotlines <- c("solid","solid","solid")
  labs <- c("2xSS 303 (C)","1xSS1xNSS 303 (D)","2xNSS 303 (E)")
  #labs <- c("Colocated 303 (A)","Hybrid 303 (B)","System Lambda (A)")
  
  for (i in 1:length(generatordflist)){
    generatordflist[[i]]$label <- names(generatordflist[i]) #may want better label
  }
  gendf <- do.call("rbind", generatordflist)
  
  #pricedf$dispatchprofit <- rep(gendf$dispatchprofit,each=12)
  pricedf$dispatchprofit <- gendf$dispatchprofit
  #pricedf$scale <- rep(100,length(pricedf$dispatchprofit))
  #scale <- 400
  #geom_line(data=pricedf, aes(x=datetime, y=dispatchprofit/100, color=label), linetype='dashed',lwd=2) +
  #  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Wind Revenue ($)")) +
  #Luke's plotting code (active)
  if (LMP==T){
    ggplot(data=pricedf, aes(x=datetime, y=LMP, color=label)) + geom_line(lwd=2) +
      theme_classic() + ylab("Price ($/MWh)") + xlab("") +
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
    ggsave(paste0("allpricesWIND",".png"), width=16, height=8)
  }
  else{
    ggplot(data=pricedf, aes(x=datetime, y=dispatchprofit, color=label)) + geom_line(lwd=2.5) +
      theme_classic() + ylab("Wind Revenue ($)") + xlab("") +
      scale_x_datetime() +
      scale_color_manual(values = plotcolors, labels=labs) +
      scale_linetype_manual(values=plotlines) +
      guides(colour=guide_legend(title="", nrow=1))+
      theme(legend.text = element_text(size=28),
            legend.title = element_text(size=30),
            legend.position = "bottom",
            plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
            axis.title.y = element_text(size=28),
            axis.text.x= element_text(size=32),
            axis.text.y= element_text(size=32),
            strip.text.x = element_text(size = 24))
    setwd(paste(baseWD, "post_processing", "figures", sep="/"))
    ggsave(paste0("allprofitWIND",".png"), width=20, height=5)
  }
}

plotDispatch <- function(results, dates, plotTitle, hours=24){
  
  dispatch <- results[["dispatch"]]
  gens <- results[["gens"]]
  offer <- results[['offer']]
  
  offer$segID <- gsub("[[:print:]]*-", "", offer[,1])
  offer$genID <- gsub("-\\d+","",offer[,1])
  offer$area <- substr(offer$Zone,start=1,stop=3)
  
  offer <- merge(offer, gens[,c("Name", "Category")], by.x="genID", by.y="Name", all.x=T)
  
  #drop duplicated entries in output
  offer <- offer[!duplicated(offer), ]
  # summarize by fuel type
  fuelemissions <- ddply(offer, ~ date + hour + area, summarise, Emissions = sum(SegmentEmissions))
  fuelemissions$area <- factor(fuelemissions$area)
  fuelemissions <- arrange(fuelemissions,area,hour)
  fuelemissions <- AddDatetime(fuelemissions)
  
  # calculate all zones
  all_emissions <- ddply(fuelemissions, ~ datetime, summarise, Emissions = sum(Emissions))
  all_emissions$area <- "All Zones"
  
  fuelemissions <- fuelemissions[,c("datetime", "Emissions", "area")]
  fuelemissions <- rbind(fuelemissions, all_emissions)
  colnames(fuelemissions) <- c("datetime","Emissions","zone")
  fuelemissions$zone <- as.factor(fuelemissions$zone)
  fuelemissions$Category <- "Coal"
  
  dispatch$zone <- substr(dispatch[,1],start=1,stop=3)
  
  # match with fuel type
  dispatch <- merge(dispatch, gens[,c("Name", "Category")], by.x="X", by.y="Name", all.x=T)
  
  #drop duplicated entries in output
  dispatch <- dispatch[!duplicated(dispatch), ]
  
  # summarize by fuel type
  fuelDispatch <- ddply(dispatch, ~ datetime + zone + Category, summarise, MW = sum(GeneratorDispatch))
  fuelDispatch$zone <- factor(fuelDispatch$zone)
  
  fuelDispatch$Category <- factor(fuelDispatch$Category, levels = c("Solar RTPV","Solar PV","Wind",
                                                                    "Oil ST","Oil CT","Gas CT","Gas CC",
                                                                    "Coal","Hydro"))
  #create color panel for later use
  dispatchcolors <- c("yellow","yellow","cyan",
                      "green","green","brown","orange","grey","blue")
  
  #drop NA's
  fuelDispatch <- fuelDispatch[!is.na(fuelDispatch$Category),]
  
  # calculate all zones
  all_dispatch <- ddply(fuelDispatch, ~ datetime + Category, summarise, MW = sum(MW))
  all_dispatch$zone <- "All Zones"
  
  fuelDispatch <- fuelDispatch[,c("datetime", "Category", "MW", "zone")]
  fuelDispatch <- rbind(fuelDispatch, all_dispatch)
  
  #Luke's plotting code (active)
  ggplot(data=fuelDispatch, aes(x=datetime, y=MW, fill=Category)) + geom_area() + 
    geom_line(data = fuelemissions, aes(x=datetime, y = Emissions,linetype=''),colour='black',size=2) +
    facet_wrap(~zone, nrow=3, scales = "free") +
    theme_classic() + ylab("Gen (MW) or Emissions (tonne)") +
    guides(fill=guide_legend(title="Gen: ", nrow=2, byrow=TRUE),
           linetype=guide_legend(title="Emissions: ")) +
    xlab("") + scale_x_datetime() + scale_fill_manual(values=dispatchcolors) +
    theme(legend.title = element_text(size=32),
          legend.text = element_text(size=28),
          legend.position = "bottom",
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=8),
          axis.text.y= element_text(size=16),
          strip.text.x = element_text(size = 24)) +
    ggtitle(paste("Generation by fuel for", plotTitle))
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("dispatch ", plotTitle, ".png"), width=40, height=12)
  
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
  #sum profit by day for plotting
  storagedf <- ddply(storagedf, ~ date + label, summarise, profit = sum(profit))
  
  #print(storagedf)
  #geom_bar(stat='identity',
  ggplot(data=storagedf, aes(x=date, y=profit, fill=label)) +
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

compareStorageOffer <- function(storagedflist, generatordflist, plotTitle="hi"){
  for (i in 1:length(generatordflist)){
    generatordflist[[i]]$label <- names(generatordflist[i])
  }
  generatordf <- do.call("rbind",generatordflist)
  generatordf <- generatordf[,c("X","datetime","SegmentOffer","label")]
  names(generatordf)[names(generatordf)=="SegmentOffer"] <- "offer"
  
  for (i in 1:length(storagedflist)){
    #storagedflist[[i]] <- storagedflist[[i]][storagedflist[[i]]$X==ss,]
    storagedflist[[i]]$label <- names(storagedflist[i]) #may want better label
  }
  storagedf <- do.call("rbind", storagedflist)
  
  dischargedf <- storagedf[,c("X","datetime","discharge_offer","label")]
  names(dischargedf)[names(dischargedf) == "discharge_offer"] <- "offer"
  dischargedf$label <- paste(dischargedf$label,"Discharge",sep=" ")
  
  chargedf <- storagedf[,c("X","datetime","charge_offer","label")]
  names(chargedf)[names(chargedf) == "charge_offer"] <- "offer"
  chargedf$label <- paste(chargedf$label,"Charge",sep=" ")
  
  #chargedf <- storagedf
  #clip and re-stack the df so charge and discharge are melted
  stacked_storagedf <- rbind(chargedf)
  
  #some helpful stuff for plotting, for now
  #scale_color_manual(values = plotcolors, labels=labs) +
  #scale_linetype_manual(values=plotlines) +
  #grab offer-related info, for plotting
  #geom_line(aes(x=datetime,y=charge_offer, color=label),linetype="dashed",lwd=2) +
  #plotcolors <- c("red","black","cyan","grey")
  plotcolors <- c("blue","black","red","grey")
  plotlines <- c("dotted","dotted","solid","solid")
  ggplot(data=stacked_storagedf, aes(x=datetime, y=offer, color=label)) + geom_line(lwd=2) +
    theme_classic() + ylab("Offer ($/MWh)") + xlab("") + ylim(0,30) +
    scale_color_manual(values = plotcolors) +
    scale_linetype_manual(values=plotlines) +
    guides(colour=guide_legend(title="", nrow=2))+
    theme(legend.text = element_text(size=34),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          plot.title = element_blank(),
          axis.title.y = element_text(size=40),
          axis.text.x= element_text(size=32),
          axis.text.y= element_text(size=32))
  
  #truncate axes for offers at some reasonable upper bound to cut off unacceptable offers
  
  #write to folder
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("OfferPlot",plotTitle,".png"), width=12, height=6)
  return("hi!")
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
  scalecolors <- c("red","black","blue")
  ggplot(data=gendf, aes(x=date, y=profit/1000, fill=label)) +
    geom_bar(stat='identity',position='dodge') + 
    theme_classic() + ylab("Monthly Profit ($K)") + xlab("") +
    scale_fill_manual(values=scalecolors) +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=32),
          legend.position = 'bottom',
          plot.title = element_blank(),
          axis.title.y = element_text(size=32),
          axis.text.x= element_blank(),
          axis.text.y= element_text(size=24)) +
    ggtitle(paste("Generator Profit ", plotTitle))
  #element_text(size = 40, face = "bold", hjust = 0.5)
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("generator profit plot",plotTitle,".png"), width=12, height=6)
  return(gendf)
}

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


cleanDispatchProfit <- function(results,dates,type='NA',filter='None',hour=24,returntype='NA'){
  gc <- results[['generatorresources']]$Gen_Index[results[['generatorresources']]$GencoIndex == 1]
  offer <- results[['offer']]
  offer <- offer[,c("X","SegmentDispatch","LMP","date","SegmentOffer","datetime")]
  nucoffer <- results[['nucoffer']]
  nucoffer <- nucoffer[,c("X","dispatch","lmp","date","offer","datetime")]
  
  colnames(nucoffer)=c("X","SegmentDispatch","LMP","date","SegmentOffer","datetime")
  offer <- rbind(offer, nucoffer)
  offer$segID <- gsub("[[:print:]]*-", "", offer[,1])
  offer$genID <- gsub("-\\d+","",offer[,1])
  offer <- offer[offer$genID==gc,]
  offer$dispatchprofit <- offer$SegmentDispatch * offer$LMP
  if (filter!='None'){
    offer <- offer[offer$genID==filter,]
  }
  DispatchProfit <- ddply(offer,~date,summarise,Profit=sum(dispatchprofit))
  if (returntype!="NA"){
    return(offer)
  }
  else{
    return(DispatchProfit)
  }
  
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
  return(objectivedf)
  
}

SumTxCongestion <- function(resultslist,plotTitle="NA",resolution="NA"){
  txlist <- list()
  pmtlist <- list()
  for (i in 1:length(resultslist)){
    resultslist[[i]]$txFlows$label <- paste(names(resultslist[i]),"Congestion",sep=" ")
    resultslist[[i]]$modelLMP$label <- paste(names(resultslist[i]),"Total",sep=" ")
  }
  if (resolution=="month"){
    pmtdf <- rbind(resultslist[[3]]$modelLMP,resultslist[[2]]$modelLMP,resultslist[[1]]$modelLMP)
    txdf <- rbind(resultslist[[3]]$txFlows,resultslist[[2]]$txFlows,resultslist[[1]]$txFlows)
    #print(txdf)
    txdf$pmt <- txdf$congestion.price.to....MWh.*txdf$flow..MWh. - txdf$congestion.price.from....MWh.*txdf$flow..MWh.
    txpmt <- ddply(txdf, ~ label, summarise, pmt = sum(pmt))
    loadpmt <- ddply(pmtdf,~label,summarise,pmt=sum(LoadPayment))
    loadpmt$pmt <- loadpmt$pmt/12
    pmts <- rbind(txpmt)
  }
  else{
    pmtdf <- rbind(resultslist[[2]]$modelLMP,resultslist[[1]]$modelLMP)
    txdf <- rbind(resultslist[[2]]$txFlows,resultslist[[1]]$txFlows)
  #print(txdf)
    txdf$pmt <- txdf$congestion.price.to....MWh.*txdf$flow..MWh. - txdf$congestion.price.from....MWh.*txdf$flow..MWh.
    txpmt <- ddply(txdf, ~ label+date, summarise, pmt = sum(pmt))
    loadpmt <- ddply(pmtdf,~label+date,summarise,pmt=sum(LoadPayment))
    loadpmt$pmt <- loadpmt$pmt/12
    pmts <- rbind(txpmt,loadpmt)
  }
  
  
  myfills <- c("red","black","blue")
  ggplot(data=pmts, aes(x=label, y=pmt/100000, fill=label)) +
    geom_bar(stat='identity',position='dodge') + 
    theme_classic() + ylab("$M") + xlab("") +
    scale_fill_manual(values=myfills) +
    guides(fill=guide_legend(title="",nrow=2)) +
    theme(legend.text = element_text(size=32),
          legend.position = 'bottom',
          plot.title = element_blank(),
          axis.title.y = element_text(size=32),
          axis.text.x= element_blank(),
          axis.text.y= element_text(size=28))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("PaymentPlot",plotTitle,".png"), width=12, height=6)
  
  return(pmts)
}


CleanLoadNetLoad <- function(results, dates, hours=24,plotTitle="NA"){

  nucoffer <- results[['nucoffer']]
  load <- results[['modelLMP']]
  #print(nucoffer)
  dailyload <- ddply(load,~date+hour,summarise,Load=sum(Load))
  dailyRE <- ddply(nucoffer,~date+hour,summarise,Dispatch=sum(dispatch))
  #dailyload$load <- dailyload$load/12
  #DispatchProfit <- ddply(nucoffer,~date,summarise,Profit=sum(dispatch))
  
  dailyload$NetLoad <- dailyload$Load - dailyRE$Dispatch
  dailyload$Load <- dailyload$Load/12
  dailyload$NetLoad <- dailyload$NetLoad/12
  
  dailyload$datetime <- as.POSIXct(paste(dailyload$date, dailyload$hour), format="%Y-%m-%d %H")
  
  PLOT_dailyload <- melt(dailyload,id.vars=c("datetime","date","hour"))
  plotcolors <- c("black","grey")
  linecolors <- c("solid","dotted")
  ggplot(data=PLOT_dailyload, aes(x=datetime, y=value, color=variable)) + geom_line(lwd=2.5) +
    theme_classic() + ylab("MW") + xlab("") +
    scale_x_datetime() + scale_color_manual(values=plotcolors)+scale_linetype_manual(values=linecolors)+
    guides(colour=guide_legend(title="", nrow=1))+
    theme(legend.text = element_text(size=36),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          plot.title = element_blank(),
          axis.title.y = element_text(size=32),
          axis.text.x= element_text(size=28),
          axis.text.y= element_text(size=28),
          strip.text.x = element_text(size = 32)) +
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
  setwd(paste(baseWD, "post_processing", "figures", sep="/"))
  ggsave(paste0("LoadandNetLoad", plotTitle, ".png"), width=12, height=5)
  return(PLOT_dailyload)
}
CleanLoadNetLoad(resultsA,dates1)

### OLD CODE TO REFERENCE ###
priceA <- plotPrices(resultsC,dates1,plotTitle='A',busfilter="303")
priceB <- plotPrices(resultsD,dates1,plotTitle='B',busfilter="303")
priceC <- plotPrices(resultsE,dates1,plotTitle='C',busfilter="303")
alist <- list(priceA,priceC)
names(alist) <- c("A","C")
glist <- list(cleanDispatchProfit(resultsC,dates1,returntype='h'),
              cleanDispatchProfit(resultsE,dates1,returntype='h'))
names(glist) <- c('A','B')
plotAllPrices(alist,glist,LMP=T)
priceA <- plotPrices(resultsA,dates2,plotTitle='SSWind',busfilter="303")
priceB <- plotPrices(resultsB,dates2,plotTitle='NSSWind',busfilter="303")
priceC <- plotPrices(resultsB,dates1,plotTitle='NSSWindbus301',busfilter="301")
alist <- list(priceA,priceB)
names(alist) <- c("A","B")

glist <- list(cleanDispatchProfit(resultsA,dates2,returntype='h'),cleanDispatchProfit(resultsB,dates2,returntype='h'))
names(glist) <- c('A','B')
plotAllPrices(alist,glist,LMP=F)

plist <- list(d4SS,d4NSS)
names(plist) <- c("A","B")
plotAllPrices(plist)
caselist <- list(cleanDispatchProfit(resultsC,dates1),cleanDispatchProfit(resultsD,dates1))
names(caselist) <- c('SS','NSS')
compareGeneratorProfit(caselist,plotTitle='1storage',resolution='month')
### END OLD CODE ###

dates1 <- seq(as.POSIXct("1/1/2019", format = "%m/%d/%Y"), by="day", length.out=31) # Configure cases period here
dates2 <- seq(as.POSIXct("1/14/2019", format = "%m/%d/%Y"), by="day", length.out=7)

resultsA <- loadResults(dates1,folder='Colocated303',subfolder="results_DA")
resultsB <- loadResults(dates1,folder='Hybrid303',subfolder="results_DA")
#resultsB2 <- loadResults(dates1,folder='303NSS_Wind303_take2',subfolder='results_da_RTVRE')
resultsC <- loadResults(dates1,folder='Wind303_2x303SS',subfolder="results_DA")
resultsD <- loadResults(dates1,folder='Colocated303_303NSS',subfolder="results_DA")
resultsE <- loadResults(dates1,folder="Wind303_2x303NSS",subfolder="results_DA")
#resultsF <- loadResults(dates1,folder="303NSS_NoWind",subfolder="results_DA_RTVRE")
#resultsF2 <- loadResults(dates1,folder='303NSS_NoWind',subfolder='results_DA_RTVRE')

#sum load payments over time interval
sum(resultsA$modelLMP$LoadPayment)/12
sum(resultsB$modelLMP$LoadPayment)/12
sum(resultsC$modelLMP$LoadPayment)/12
sum(resultsD$modelLMP$LoadPayment)/12
sum(resultsE$modelLMP$LoadPayment)/12

#portfolio generation profits, compared
caselist <- list(cleanDispatchProfit(resultsA,dates1),cleanDispatchProfit(resultsB,dates1))
names(caselist) <- c('Colocated (A)','Hybrid (B)')
compareGeneratorProfit(caselist,plotTitle='HybridProfit',resolution='NA')

#price streams
priceA <- plotPrices(resultsA,dates1,plotTitle='A',busfilter="303")
priceB <- plotPrices(resultsB,dates1,plotTitle='B',busfilter="303")
priceC <- plotPrices(resultsA,dates1,plotTitle='C',busfilter="301")
alist <- list(priceA,priceB,priceC)
names(alist) <- c("A","B","C")
glist <- list(cleanDispatchProfit(resultsA,dates1,returntype='h'),
              cleanDispatchProfit(resultsB,dates1,returntype='h'),
              cleanDispatchProfit(resultsA,dates1,returntype='h'))
names(glist) <- c('A','B','C')
plotAllPrices(alist,glist,LMP=T)

#offer streams
caselist <- list(plotStorage(resultsA,dates1,plotTitle="A"),plotStorage(resultsB,dates1,plotTitle="B"))
names(caselist) <- c('ESR Colocated','ESR Hybrid')

genlist <- list(cleanDispatchProfit(resultsA,dates1,returntype = "offer"),cleanDispatchProfit(resultsB,dates1,returntype = "offer"))
names(genlist) <- c("Wind Colocated","Wind Hybrid")
compareStorageOffer(caselist,genlist,plotTitle="charge")

#load (and/or congestion) payments
rlist <- list(resultsA,resultsB)
names(rlist) <- c("Colocated","Hybrid")
SumTxCongestion(rlist)

### cases C-E ###
#portfolio generation profits, compared
caselist <- list(cleanDispatchProfit(resultsC,dates1),cleanDispatchProfit(resultsD,dates1),
                 cleanDispatchProfit(resultsE,dates1))
names(caselist) <- c('(C) 2xSS','(D) 1xSS1xNSS',"(E) 2xNSS")
compareGeneratorProfit(caselist,plotTitle='SSProfit',resolution='month')

#price streams
priceC <- plotPrices(resultsC,dates1,plotTitle='C',busfilter="303")
priceD <- plotPrices(resultsD,dates1,plotTitle='D',busfilter="303")
priceE <- plotPrices(resultsE,dates1,plotTitle='E',busfilter="303")
alist <- list(priceC,priceD,priceE)
names(alist) <- c("C","D","E")
glist <- list(cleanDispatchProfit(resultsC,dates1,returntype='h'),
              cleanDispatchProfit(resultsD,dates1,returntype='h'),
              cleanDispatchProfit(resultsE,dates1,returntype='h'))
names(glist) <- c('C','D','E')
plotAllPrices(alist,glist,LMP=T)

#offer-related figures
caselist <- list(plotStorage(resultsC,dates1,plotTitle="C"),
                 plotStorage(resultsD,dates1,plotTitle="D"),
                 plotStorage(resultsE,dates1,plotTitle="E"))
names(caselist) <- c('ESR 2xSS (C)','ESR 1xSS1xNSS (D)', 'ESR 2xNSS (E)')

genlist <- list(cleanDispatchProfit(resultsC,dates1,returntype = "offer"),
                cleanDispatchProfit(resultsD,dates1,returntype = "offer"),
                cleanDispatchProfit(resultsE,dates1,returntype = "offer"))
names(genlist) <- c("Wind C","Wind D","Wind E")
compareStorageOffer(caselist,genlist,plotTitle="chargeCDE")

#load and congestion pmts
rlist <- list(resultsC,resultsD,resultsE)
names(rlist) <- c('(C) 2xSS','(D) 1xSS1xNSS',"(E) 2xNSS")
SumTxCongestion(rlist,resolution="month",plotTitle="CDE")

### end cases C-E ###

caselist <- list(resultsA, resultsB2)
names(caselist) <- c('SSwind',"NSSWind")
#names(caselist) <- c('NSS',"SS",'mix')
df5 <- compareObjectives(caselist)
df4$delta <- df2$SSProfit-df2$Objective
write.csv(df2,"df2.csv")


#plotDispatch(results2,dates2,plotTitle='Feb',F)

d1 <- plotDispatch(results1,dates1,plotTitle='Jan 1 2019 RTVRE')
d2NSS <- plotPrices(results1,dates1,plotTitle='July 28 NSS')
d3 <- plotStorage(results1,dates1,plotTitle='July 28 A')

d1bind <- plotDispatch(results2,dates1,plotTitle='Jan 1 2019 BIND DA')
d2SS <- plotPrices(results3,dates1,plotTitle='July 28 2SS')
d3bind <- plotStorage(results2,dates1,plotTitle='July 28 B')

d4NSS <- plotPrices(results4, dates1, plotTitle="July 28 NSS WIND")
d4SS <- plotPrices(results5, dates1, plotTitle="July 28 SS WIND")

#compareplotDispatch(d1,d1RT)
#compareplotPrices(d2,d2RT)
#compareplotStorage(d3,d3RT)

d1 <- plotDispatch(results3,dates1,plotTitle='SSWind')

d3 <- plotStorage(results3,dates1,plotTitle='SSWind')

d1NSS <- plotDispatch(results1,dates1,plotTitle='NSSWind')

d3NSS <- plotStorage(results1,dates1,plotTitle='NSSWind')

compareplotDispatch(d1,d1NSS)

priceA <- plotPrices(resultsA,dates1,plotTitle='SSWind_')
priceB <- plotPrices(resultsB,dates1,plotTitle='NSSWind_')
compareplotPrices(priceA,d2bind)
compareplotStorage(d3,d3bind)


caselist <- list(d3,d3)
names(caselist) <- c('day-ahead','real-time')
compareStorageHeatplot(caselist)
compareStorageHeatplot(caselist,type='lmp')
compareStorageProfit(caselist,plotTitle='test',resolution='month')

caselist <- list(cleanDispatchCost(results1,dates1),cleanDispatchCost(results3,dates1))
names(caselist) <- c('NSS','SS')
compareTotalGeneratorCost(caselist,plotTitle='test',resolution='month')

caselist <- list(cleanDispatchCost(resultsC,dates1,type="lmp"),
                 cleanDispatchCost(resultsD,dates1,type="lmp"))
names(caselist) <- c('SS','NSS')

#cleanDispatchCost(results1,dates1)
compareTotalGeneratorCost(caselist,plotTitle='LMPTestwind',resolution='month')

caselist <- list(cleanDispatchProfit(results1,dates1),cleanDispatchProfit(results2,dates1))
names(caselist) <- c('day-ahead','real-time')
compareGeneratorProfit(caselist,plotTitle='test')

### end

### old code function examples

## January only ##
datesJan <- seq(as.POSIXct("1/3/2019", format = "%m/%d/%Y"), by="day", length.out=2)#31

nostorageJan <- loadAllCases(datesJan,folder='NoStorage')
baseJan <- loadAllCases(datesJan,folder='base')
competitiveJan <- loadAllCases(datesJan,folder="competitive")
baseCO2Jan <- loadAllCases(datesJan,folder='baseCO230')
competitiveCO2Jan <- loadAllCases(datesJan,folder='competitiveCO230')
competitiveCCJan <- loadAllCases(datesJan,folder='competitive300CCs')
competitiveSteamJan <- loadAllCases(datesJan,folder='competitive300STEAM')

base303Jan <- loadAllCases(datesJan,folder='BaseStorageBus303')
base309Jan <- loadAllCases(datesJan,folder='BaseStorageBus309')
competitive303Jan <- loadAllCases(datesJan,folder='CompetitiveStorageBus303')
competitive309Jan <- loadAllCases(datesJan,folder='CompetitiveStorageBus309')
competitive303andWind309Jan <- loadAllCases(datesJan,folder='CompetitiveStorageBus303andWind309')

#storage-related plots
#nostorageJan,baseJan,competitiveJan,baseCO2Jan,
#competitiveCO2Jan,competitiveCCJan,competitiveSteamJan

caselist <- list(plotStorage(nostorageJan,datesJan,plotTitle='1'),
                 plotStorage(baseJan,datesJan,plotTitle='2'),
                 plotStorage(competitiveJan,datesJan,plotTitle='3'),
                 plotStorage(base303Jan,datesJan,plotTitle='8'),
                 plotStorage(base309Jan,datesJan,plotTitle='9'),
                 plotStorage(competitive303Jan,datesJan,plotTitle='10'),
                 plotStorage(competitive309Jan,datesJan,plotTitle='11'),
                 plotStorage(competitive303andWind309Jan,datesJan,plotTitle='12'))
names(caselist) <- c('NoStorage','Base313','Comp313',
                     'Base303','Base309','Comp303','Comp309',
                     'Comp303Wind309')

compareStorageProfit(caselist,plotTitle='Jan 3-4th',resolution='na')#resolution='month'
compareStorageHeatplot(caselist,plotTitle='Jan 4th')
compareStorageHeatplot(caselist,plotTitle='Jan',type='lmp')

#lmp-related plots

#generator cost, revenue, and profit
#cost
myfilter ='None'#316_STEAM_1#309_WIND_1
caselist <- list(cleanDispatchCost(nostorageJan,datesJan,filter=myfilter),
                 cleanDispatchCost(baseJan,datesJan,filter=myfilter),
                 cleanDispatchCost(competitiveJan,datesJan,filter=myfilter),
                 cleanDispatchCost(base303Jan,datesJan,filter=myfilter),
                 cleanDispatchCost(base309Jan,datesJan,filter=myfilter),
                 cleanDispatchCost(competitive303Jan,datesJan,filter=myfilter),
                 cleanDispatchCost(competitive309Jan,datesJan,filter=myfilter),
                 cleanDispatchCost(competitive303andWind309Jan,datesJan,filter=myfilter))
names(caselist) <- c('NoStorage','Base','Competitive','Base303','Base309',
                     'Competitive303','Competitive309','Competitive303andWind309')
compareTotalGeneratorCost(caselist,plotTitle='JanCost',resolution='na')#resolution='month'

#payments
#cleanDispatchCost(baseCO2Jan,datesJan,type='lmp')#filter='316_STEAM_1'
myfilter='309_WIND_1'
caselist <- list(cleanDispatchCost(nostorageJan,datesJan,type='lmp',filter=myfilter),
                 cleanDispatchCost(baseJan,datesJan,type='lmp',filter=myfilter),
                 cleanDispatchCost(competitiveJan,datesJan,type='lmp',filter=myfilter),
                 cleanDispatchCost(base303Jan,datesJan,type='lmp',filter=myfilter),
                 cleanDispatchCost(base309Jan,datesJan,type='lmp',filter=myfilter),
                 cleanDispatchCost(competitive303Jan,datesJan,type='lmp',filter=myfilter),
                 cleanDispatchCost(competitive309Jan,datesJan,type='lmp',filter=myfilter),
                 cleanDispatchCost(competitive303andWind309Jan,datesJan,type='lmp',filter=myfilter))
names(caselist) <- c('NoStorage','Base313','Comp313','Base303','Base309',
                     'Comp303','Comp309','Comp303Wind309')
compareTotalGeneratorCost(caselist,plotTitle='JanPmt',resolution='m')#resoultion='month'
#require(scales)
#emissions
caselist <- list(cleanEmissions(nostorageJan,datesJan),
                 cleanEmissions(baseJan,datesJan),
                 cleanEmissions(competitiveJan,datesJan),
                 cleanEmissions(base303Jan,datesJan),
                 cleanEmissions(base309Jan,datesJan),
                 cleanEmissions(competitive303Jan,datesJan),
                 cleanEmissions(competitive309Jan,datesJan),
                 cleanEmissions(competitive303andWind309Jan,datesJan))
names(caselist) <- c('NoStorage','Base313','Comp313','Base303','Base309',
                     'Comp303','Comp309','Comp303Wind309')
compareTotalEmissions(caselist,plotTitle='Jan',resolution='m')