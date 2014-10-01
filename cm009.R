library(assertthat)

gd_url <- "http://tiny.cc/gapminder"
gDat <- read.delim(file = gd_url)
str(gDat)

## Find function "qdiff4" = "max" - "min"

quantile(gDat$lifeExp,probs=c(0,1))

qdiff4 <- function(x, probs=c(0,1)){
  assert_that(is.numeric(x))
  the_quantiles <- quantile(x,probs)
  names(the_quantiles) <- NULL
  return(max(the_quantiles)-min(the_quantiles))
}
qdiff4(gDat$lifeExp)


## NAs (what if missing data?)
z <- gDat$lifeExp
z[3] <- NA
head(z)
quantile(z)
mean(z)
#  what can we do? "na.rm=TRUE"
quantile(z, na.rm=TRUE)

qdiff5 <- function(x, probs=c(0,1), na.rm=TRUE){
  assert_that(is.numeric(x))
  the_quantiles <- quantile(x,probs, na.rm)
  names(the_quantiles) <- NULL
  return(max(the_quantiles)-min(the_quantiles))
}
qdiff5(gDat$lifeExp)
#  what if without "na.rm=TRUE"
qdiff5(z)
qdiff4(z)


## the "..." argument

