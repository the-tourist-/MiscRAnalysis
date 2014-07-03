require(RODBC)

EOD.Price.Jumps <- read.csv("~/GitHub/MiscRAnalysis/EOD Price Jumps.csv", stringsAsFactors=FALSE)
EOD.Price.Jumps$AvgVolume = as.numeric(EOD.Price.Jumps$AvgVolume)
EOD.Price.Jumps$StdevVolume = as.numeric(EOD.Price.Jumps$StdevVolume)
EOD.Price.Jumps$Close = as.numeric(EOD.Price.Jumps$Close)
EOD.Price.Jumps$AbsChange = as.numeric(EOD.Price.Jumps$AbsChange)
EOD.Price.Jumps$VolScore = as.numeric(EOD.Price.Jumps$VolScore)
EOD.Price.Jumps$VolumeSpike = as.numeric(EOD.Price.Jumps$VolumeSpike)
EOD.Price.Jumps$Date = as.Date(EOD.Price.Jumps$Date, "%m/%d/%Y")
EOD.Price.Jumps$PrevDate = as.Date(EOD.Price.Jumps$PrevDate, "%m/%d/%Y")
EOD.Price.Jumps <- cbind(Suggestion=rep("", length(EOD.Price.Jumps$EntityId)), EOD.Price.Jumps)
dev.off(dev.list()["RStudioGD"])
par(mar=c(2,3,3,2))
par(mfrow=c(4,3))
for (i in 1:length(EOD.Price.Jumps$Symbol)) {
  with (EOD.Price.Jumps, {
    #SubTitle <- 
    #  sprintf("%s: Date=%s (%s), Close=%#.2f (%#.2f), Multiple=%s, Volatility=%i, Volume Spike=%#.1f, Volume=%i", 
    #          Symbol[i], Date[i], PrevDate[i], AdjClose[i], PrevAdjClose[i], 
    #          paste(ifelse(LogChange[i]>0, "", "1/"), as.character(sprintf("%#.1f", AbsChange[i])), sep=""),
    #          as.integer(VolScore[i]), VolumeSpike[i], as.integer(AdjVolume[i]))
    #print(SubTitle)
    MIPlotEODDataPrices(EntityId[i], MarkDate=Date[i])
  })
}