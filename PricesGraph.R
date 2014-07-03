require(RODBC)
require(stats)

MIPlotPrices <- function(EntityId=-1, Symbol="", SubTitle=NULL, MarkDate=NULL) {
  MDC <- odbcConnect("MindlessInvestingPROD", believeNRows=T)
  if (EntityId == -1)
    EntityId = sqlQuery(MDC, sprintf("select dbo.GetEI('%s')", Symbol))[[1]]
  if (is.na(EntityId)) {
    print(sprintf("Symbol %s not found", Symbol))
    return()
  }
  
  Entity <- sqlQuery(MDC, sprintf("select * from Entities where EntityId = %i", EntityId))
  PriceData <- sqlQuery(MDC, sprintf(
    "select Date, AdjClose, [Close], LogChange, AdjVolume
    from Prices as P 
    where P.EntityId = %i
    order by Date"
    , EntityId, stringsAsFactors=F))
  odbcClose(MDC)
  
  if (class(PriceData) == "character") {
    print(PriceData)
    return()
  }
  
  if (length(PriceData$Date) == 0)
    return()
  
  PriceData$Date <- as.Date(PriceData$Date)
  PriceData$AdjClose <- as.numeric(PriceData$AdjClose)
  PriceData$Close <- as.numeric(PriceData$Close)
  PriceData$LogChange <- as.numeric(PriceData$LogChange)
  
  with (PriceData, {
    
    # Basic Plot
    Title <- paste(Entity$Symbol, Entity$Name, sep=" - ")
    if (!is.null(SubTitle))
      Title <- paste(Title, SubTitle, sep="\n")
    plot(Date, AdjClose, type="n", log="y", xaxt="n", yaxt="n", main=Title, xlab=NA, ylab=NA)
    
    # X Axis and Gridlines
    PriceRange <- floor(log10(range(AdjClose)))
    Pow <- seq(PriceRange[1], PriceRange[2]+1)
    TicksAt <- as.vector(sapply(Pow, function(p) (1:10)*10^p))
    for (Marker in c(1.5,2,3,4,5,7))
      Pow <- c(Pow, seq(PriceRange[1], PriceRange[2]+1)+log(Marker, 10)-1)
    Labels <- signif(10^Pow, 2)
    axis(2, 10^Pow, las=2, labels=Labels)
    axis(2, TicksAt, tcl=-0.25, lwd=0, lwd.ticks=1, labels=NA)
    abline(h=TicksAt, col="lightgrey")
    abline(h=10^Pow, col="darkgrey", lwd=(Pow==as.integer(Pow))+1)
    lines(Date, Close, col="orange", lwd=2)
    lines(Date, AdjClose, col="blue", lwd=2)
    
    # Y Axis and Gridlines
    NoOfDays <- as.numeric(max(Date)-min(Date))
    
    if (NoOfDays > 3 * 365) {
      DateTicksAt <- as.Date(paste(seq(as.numeric(format(min(Date), "%Y")), as.numeric(format(max(Date), "%Y"))+1), "-01-01", sep=""))
      axis.Date(1, x=Date, at=DateTicksAt, format="%y")
      #abline(which(Date[Date==DateTicksAt])), col="darkgrey")
    }
    else
      axis.Date(1, x=Date)
    
    # Colour large price moves red
    SuspiciousLogChanges <- which(abs(LogChange)>sqrt(2)-1)
    segments(Date[SuspiciousLogChanges-1], AdjClose[SuspiciousLogChanges-1], Date[SuspiciousLogChanges], AdjClose[SuspiciousLogChanges], col="red", lwd=2)
    
    # Place line on graph according to the mark date
    for (MD in MarkDate) {
      MarkDateIndex = which(Date==MD)
      segments(Date[MarkDateIndex-1], AdjClose[MarkDateIndex-1], Date[MarkDateIndex], AdjClose[MarkDateIndex], col="green", lwd=2)
    }
    
  })
}

MIPlotPrices(Symbol="AMPI3")