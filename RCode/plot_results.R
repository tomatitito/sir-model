library(ggplot2)
library(dplyr)
library(purrr)

path = "data/season.csv"
dat <- read.csv(path,colClasses=c("numeric", "numeric", "factor"),header=FALSE)

## dat needs three columns:
## week, cases, simulation-index
names(dat) <- c("week", "cases", "sim_id")

hdi = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = data.frame( lo=HDImin , hi=HDImax )
  return( HDIlim )
}

borders <- dat %>%
  split(.$week) %>%
  map_df(~hdi( .$cases, 0.95)) %>%
  cbind(week=0:max(dat$week))


p <- ggplot(data=dat, aes(x=week, y=cases)) +
  geom_point(alpha=1/10) + #geom_smooth(aes(group=1))
  geom_ribbon(data=borders, aes(week, ymin=lo, ymax=hi), fill="blue", alpha=1/10, inherit.aes=FALSE)


ggsave(file="season.pdf")
