require(RODBC)
require(stats)

MIPlotReutersPITPrices <- function(EntityId=-1, Symbol="", SubTitle=NULL, MarkDate=NULL) {
  MDC <- odbcConnect("MindlessInvestingPROD", believeNRows=T)
  RDC <- odbcConnect("ReutersPIT", believeNRows=T)
  
  if (EntityId == -1)
    EntityId = sqlQuery(MDC, sprintf("select dbo.GetEI('%s')", Symbol))[[1]]
  if (is.na(EntityId)) {
    print(sprintf("Symbol %s not found", Symbol))
    return()
  }
  
  Entity <- sqlQuery(MDC, sprintf("select * from Entities where EntityId = %i", EntityId))
  LatestAdjustmentFactor <- as.numeric(sqlQuery(RDC, sprintf("select top 1 adjustment_factor from RF_Daily_Pr where Repno = '%s' and issue_id = %i and adjustment_factor is not null order by point_date desc", Entity$Repno, Entity$IssueId), stringsAsFactors=F))
  if (is.na(LatestAdjustmentFactor))
    LatestAdjustmentFactor <- 1
  
  PriceData <- sqlQuery(RDC, sprintf(
    "select P.point_date as Date
    , P.close_price * nullif(coalesce(AP.adjustment_factor, 1), 0) / nullif(%f, 0) as AdjClose, P.close_price as [Close]
    , log(nullif(P.close_price, 0) * nullif(coalesce(AP.adjustment_factor, 1), 0) / nullif(PP.close_price, 0) / nullif(coalesce(PP.adjustment_factor, AP.adjustment_factor, 1), 0)) as LogChange
    from RF_Daily_Pr as P 
    left outer join RF_Daily_Pr as PP 
    on PP.Repno = P.Repno 
    and PP.issue_id = P.issue_id 
    and PP.point_date = (
      select max(point_date) 
      from RF_Daily_Pr 
      where Repno = P.Repno 
      and issue_id = P.issue_id 
      and point_date < P.point_date)   
    left outer join RF_Daily_Pr as AP 
    on AP.Repno = P.Repno 
    and AP.issue_id = P.issue_id 
    and AP.point_date = (
      select max(point_date) 
      from RF_Daily_Pr 
      where Repno = P.Repno 
      and issue_id = P.issue_id 
      and point_date <= P.point_date 
      and adjustment_factor is not null)   
    where P.Repno = '%s' 
    and P.issue_id = %i 
    and P.close_price is not null
    order by P.point_date"
  , LatestAdjustmentFactor, Entity$Repno, Entity$IssueId), stringsAsFactors=F)
  odbcClose(MDC)
  
  if (class(PriceData) == "character") {
    print(PriceData)
    return()
  }
  
  PriceData$Date <- as.Date(PriceData$Date)
  PriceData$AdjClose <- as.numeric(PriceData$AdjClose)
  PriceData$Close <- as.numeric(PriceData$Close)
  PriceData$LogChange <- as.numeric(PriceData$LogChange)
  
  if (is.na(min(PriceData$AdjClose))) {
    print("No Log Changes Found")
    PriceData$AdjClose <- PriceData$Close
  }
  
  if (length(PriceData$Date) == 0)
    return()
  
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

MIPlotReutersPITPrices(Symbol="ABII", MarkDate=as.Date(c("2007-11-14", "2007-11-15")))