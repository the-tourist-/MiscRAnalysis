require(RODBC)

Price.Jumps <- read.csv("~/GitHub/MiscRAnalysis/Price Jumps Around Splits.csv", stringsAsFactors=FALSE)
Price.Jumps$LogChange = as.numeric(Price.Jumps$LogChange)
Price.Jumps$LogSplitRatio = as.numeric(Price.Jumps$LogSplit)
Price.Jumps$Close = as.numeric(Price.Jumps$Close)
Price.Jumps$AdjClose = as.numeric(Price.Jumps$AdjClose)
Price.Jumps$PrevAdjClose = as.numeric(Price.Jumps$PrevAdjClose)
Price.Jumps$LogSplitRatio = as.numeric(Price.Jumps$LogSplitRatio)
Price.Jumps$VolumeSpike = as.numeric(Price.Jumps$VolumeSpike)
Price.Jumps$Date = as.Date(Price.Jumps$Date, "%m/%d/%Y")
Price.Jumps$PrevDate = as.Date(Price.Jumps$PrevDate, "%m/%d/%Y")
Price.Jumps$Recommendation <- rep(" ", times=length(Price.Jumps$Symbol))

if (!is.null(dev.list()))
  dev.off(dev.list())

for (i in 1:length(Price.Jumps$Symbol)) {
  if(Price.Jumps$Recommendation[i] != " " & nchar(Price.Jumps$Recommendation[i]) == 1)
    next()
  
  with (Price.Jumps, {
    par(mfcol=c(3,1))
    
    SubTitle=sprintf("%s - LogChange = %#.2f, LogSplitRatio=%#.1f", PriceSourceUser[i], LogChange[i], LogSplitRatio[i])
    
    print("")
    print(sprintf("%i: %s - %s",i, Price.Jumps$Symbol[i], Price.Jumps$Name[i]))
    print(SubTitle)
    
    tryCatch({MIPlotReutersPITPrices(EntityId=EntityId[i], MarkDate=c(Date[i]))}, error= function(err) { print(err)})
    tryCatch({MIPlotPrices(EntityId=EntityId[i], MarkDate=c(Date[i]), SubTitle=SubTitle)}, error= function(err) { print(err)})
    tryCatch({MIPlotEODDataPrices(EntityId=EntityId[i], MarkDate=c(Date[i]))}, error= function(err) { print(err)})
  })
  print ("")
  
  key <- toupper(scan(n=1, what="character"))
  if (length(key) == 1) {
    if (key == "Q")
      return(NULL)
    else if (key == "P")
      i <- i - 2
    else
      Price.Jumps$Recommendation[i] = key
  }
}