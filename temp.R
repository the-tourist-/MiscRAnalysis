require(RODBC)

#Price.Jumps <- read.csv("~/GitHub/MiscRAnalysis/Price Jumps.csv", stringsAsFactors=FALSE)
#Price.Jumps$LogChange1 = as.numeric(Price.Jumps$LogChange1)
#Price.Jumps$LogChange2 = as.numeric(Price.Jumps$LogChange2)
#Price.Jumps$Close1 = as.numeric(Price.Jumps$Close1)
#Price.Jumps$Close2 = as.numeric(Price.Jumps$Close2)
#Price.Jumps$SplitFactor1 = as.numeric(Price.Jumps$SplitFactor1)
#Price.Jumps$SplitFactor2 = as.numeric(Price.Jumps$SplitFactor2)
#Price.Jumps$VolumeSpike = as.numeric(Price.Jumps$VolumeSpike)
#Price.Jumps$Multiplier = as.numeric(Price.Jumps$Multiplier)
#Price.Jumps$Date1 = as.Date(Price.Jumps$Date1, "%m/%d/%Y")
#Price.Jumps$Date2 = as.Date(Price.Jumps$Date2, "%m/%d/%Y")
#Price.Jumps$Recommendation <- rep(" ", times=length(Price.Jumps$Symbol))

if (!is.null(dev.list()))
  dev.off(dev.list())

for (i in 1:length(Price.Jumps$Symbol)) {
  if(Price.Jumps$Recommendation[i] != "F" & nchar(Price.Jumps$Recommendation[i]) == 1)
    next()
  if (Price.Jumps$SplitFactor1[i] != Price.Jumps$SplitFactor2[i])
    next()
    
  with (Price.Jumps, {
    par(mfcol=c(3,1))
    
    SubTitle=sprintf("%s - Multiplier = %#.2f, VolumeSpike=%#.1f", PriceSourceUser[i], Multiplier[i], VolumeSpike[i])
    if (SplitFactor1[i] != SplitFactor2[i])
      SubTitle <- paste(SubTitle, sprintf("SplitFactors of (%#.3f, %#.3f)", SplitFactor1[i], SplitFactor2[i]))
    
    print("")
    print(sprintf("%i: %s - %s",i, Price.Jumps$Symbol[i], Price.Jumps$Name[i]))
    print(SubTitle)
    
    tryCatch({MIPlotReutersPITPrices(EntityId=EntityId[i], MarkDate=c(Date1[i], Date2[i]))}, error= function(err) { print(err)})
    tryCatch({MIPlotPrices(EntityId=EntityId[i], MarkDate=c(Date1[i], Date2[i]), SubTitle=SubTitle)}, error= function(err) { print(err)})
    tryCatch({MIPlotEODDataPrices(EntityId=EntityId[i], MarkDate=c(Date1[i], Date2[i]))}, error= function(err) { print(err)})
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