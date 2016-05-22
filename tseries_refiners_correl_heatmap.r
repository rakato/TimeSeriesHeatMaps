library(xts)
library(Quandl)

my_start_date <- "2013-01-05"
VLO <- Quandl("YAHOO/VLO", start_date = my_start_date, type = "xts")
TSO <- Quandl("YAHOO/TSO", start_date = my_start_date, type = "xts")
HFC <- Quandl("YAHOO/HFC", start_date = my_start_date, type = "xts")
WNR <- Quandl("YAHOO/WNR", start_date = my_start_date, type = "xts")
MPC <- Quandl("YAHOO/MPC", start_date = my_start_date, type = "xts")
CVI <- Quandl("YAHOO/CVI", start_date = my_start_date, type = "xts")
PBF <- Quandl("YAHOO/PBF", start_date = my_start_date, type = "xts")


#Get closing prices
VLOc <- VLO[,"Close"]
TSOc <- TSO[,"Close"]
HFCc<- HFC[,"Close"]
WNRc <- WNR[,"Close"]
MPCc <- MPC[,"Close"]
CVIc <- CVI[,"Close"]
PBFc <- PBF[,"Close"]

# The xts merge(.) function will only accept two series at a time.
# We can, however, merge multiple columns by downcasting to *zoo* objects.
# Remark:  "all = FALSE" uses an inner join to merge the data.
z <- merge(as.zoo(VLOc), as.zoo(TSOc), as.zoo(HFCc), as.zoo(WNRc),
           as.zoo(MPCc), as.zoo(CVIc),as.zoo(PBFc),  all = FALSE)

# Set the column names; these will be used in the heat maps:
myColnames <- c("VLO","TSO","HFC","WNR","MPC","CVI","PBF")
colnames(z) <- myColnames

# Cast back to an xts object:
mktPrices <- as.xts(z)

# Get returns
mktRtns <- diff(log(mktPrices), lag = 1)
head(mktRtns)
mktRtns <- mktRtns[-1, ]  # Remove 1st row NA



require(gplots)

generate_heat_map <- function(correlationMatrix, title)
{
  
  heatmap.2(x = correlationMatrix,		# the correlation matrix input
            cellnote = correlationMatrix,	# places correlation value in each cell
            main = title,			# heat map title
            symm = TRUE,			# configure diagram as standard correlation matrix
            dendrogram="none",		# do not draw a row dendrogram
            Rowv = FALSE,			# keep ordering consistent
            trace="none",			# turns off trace lines inside the heat map
            density.info="none",		# turns off density plot inside color legend
            notecol="black")		# set font color of cell labels to black
  
}


#We round the numbers to cut off decimal point places and create a couple diff corr maps for last years
corr1 <- round(cor(mktRtns) * 100,2)
corr2 <- round(cor(mktRtns['2015-01']) * 100, 2)
corr3 <- round(cor(mktRtns['2016-01']) * 100, 2)

generate_heat_map(corr1, "Refiners Correlation Matrix 2013-Present")
generate_heat_map(corr2, "Refiners Correlation Matrix 2015")
generate_heat_map(corr3, "Refiners Correlation Matrix 2016")



