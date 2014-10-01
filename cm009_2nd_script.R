#install.packages("plyr)
library(plyr)
library(dplyr)
library(ggplot2)
gd_url <- "http://tiny.cc/gapminder"
gDat <- read.delim(file = gd_url)
str(gDat)


## challenge

## input is a data.frame with at least two variables, one named year, one named lifeExp
## output is a numeric vector= est. intercept and slope from lm(lifeExp~year,data)


jCountry <- "France" # pick, but do not hard wire, an example
(jDat <- subset(gDat, country == jCountry)) # temporary measure!

xyplot(lifeExp ~ year, jDat, type = c("p", "r")) # always plot the data
p <- ggplot(j_dat, aes(x=year, y=lifeExp))
p+geom_point()

jFit <- lm(lifeExp ~ year, jDat)
coef(jFit)


jFun <- function(x) {
  yearMin <- min(gDat$year)
  estCoefs <- coef(lm(lifeExp ~ I(year-yearMin), x))
  names(estCoefs) <- c("intercept", "slope")
  return(estCoefs)
}
jFun(jDat) # trying out our improved function ... yes still get same numbers
