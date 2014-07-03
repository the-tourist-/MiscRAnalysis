trigraph <- function(Symbol="", EntityId=-1, SubTitle=NULL, MarkDate=NULL) {
  par(mfcol=c(3,1))
  MIPlotReutersPITPrices(EntityId, Symbol, SubTitle, MarkDate)
  MIPlotPrices(EntityId, Symbol, SubTitle, MarkDate)
  MIPlotEODDataPrices(EntityId, Symbol, SubTitle, MarkDate)
}

