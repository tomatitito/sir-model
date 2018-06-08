library(ggplot2)
library(dplyr)
library(purrr)
library(scales)
library(tidyr)

#args <- commandArgs(TRUE)

## NOTE: when naming cols or changing types 
##the way the data looks has changed
## older versions have three columns:
## week, cases, simulation-index
## newer versions have more
## week, new, S, I, R, primary, secondary, sim_id

#path = "data/2018-01-31T22:37:09.770_two-stage-poisson-query_80000000_5000_1000.csv"
#path <- args[1]
# dat <- read.csv(path,colClasses=c("numeric", "numeric","factor"),header=TRUE)
dat <- read.csv(path,colClasses="numeric",header=TRUE) %>% mutate(cases=new )


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

# borders <- dat %>%
#   split(.$week) %>%
#   map_df(~hdi( .$cases, 0.95)) %>%
#   cbind(week=0:max(dat$week))

borders = function(data){
  data %>%
    split(.$week) %>%
    map_df(~hdi( .$cases, 0.95)) %>%
    cbind(week=0:max(dat$week))
}

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# higher order function to compute mean, mode and median
compute_stat = function(df, f){
  df %>%
    split(.$week) %>%
    map_dbl(~f(.$new)) 
}

# layer data for aggregate statistics
stats_wide = data.frame(week=0:39,means=compute_stat(dat, mean), medians=compute_stat(dat, median), modes=compute_stat(dat, mode))
stats_long = gather(stats_wide, key=stat, value=val, means:modes)

# p_new <- ggplot(data=dat, aes(x=week, y=cases)) +
#   geom_point(alpha=1/100) +
#   geom_ribbon(data=borders, aes(week, ymin=lo, ymax=hi), fill="blue", alpha=1/10, inherit.aes=FALSE) +
#   scale_y_continuous(labels = comma) +
#   labs(title="Simulierte Neuerkrankungen ueber 40 Wochen", x="Woche", y="Neuerkrankungen") +
#   geom_line(data=stats_wide, aes(x=1:nrow(stats_wide),y=means, color="mean"), inherit.aes=FALSE) +
#   geom_line(data=stats_wide, aes(x=1:nrow(stats_wide),y=medians, color="median"), inherit.aes=FALSE) +
#   geom_line(data=stats_wide, aes(x=1:nrow(stats_wide),y=modes, color="mode"), inherit.aes=FALSE) +
#   theme(legend.title=element_blank())

season_plot = function(data){
  ggplot(data=data, aes(x=week, y=cases)) +
    geom_point(alpha=1/100) +
    geom_ribbon(data=borders_f(data), aes(week, ymin=lo, ymax=hi), fill="blue", alpha=1/10, inherit.aes=FALSE)
}

# p_stats <- ggplot(stats_long, aes(x=week,y=val, color=stat)) + geom_point(show.legend=FALSE) +
#   facet_grid(stat ~ ., scales="free") +
#   theme(axis.title.x=element_blank(), axis.title.y=element_blank())
#
# weekly_dists <- ggplot(data=dat, aes(cases)) +
#   geom_histogram() +
#   facet_wrap(~ week, ncol=7) +
#   theme(axis.text.x=element_text(size=5)) +
#   labs(title="Verteilung der simulierten Neuerkrankungen pro Woche")
#
# weekly_dists_free <- ggplot(data=dat, aes(cases)) +
#   geom_histogram() +
#   facet_wrap(~ week, ncol=7, scales="free") +
#   theme(axis.text.x=element_text(size=5)) +
#   labs(title="Verteilung der simulierten Neuerkrankungen pro Woche")
#
# ggsave(file="season_new.pdf", plot=p_new, height=5, width=10)
# ggsave(file="stats_new.pdf", plot=p_stats, height=2.5, width=7)
# ggsave(file="weekly_dists_new.pdf", plot=weekly_dists, height=9, width=11)
# ggsave(file="weekly_dists_free_new.pdf", plot=weekly_dists_free, height=9, width=11)
