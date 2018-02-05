library(ggplot2)
library(dplyr)
library(purrr)
library(scales)

args <- commandArgs(TRUE)

## NOTE: when naming cols or changing types 
##the way the data looks has changed
## older versions have three columns:
## week, cases, simulation-index
## newer versions have more
## week, new, S, I, R, primary, secondary, sim_id

#path = "data/2018-01-31T22:37:09.770_two-stage-poisson-query_80000000_5000_1000.csv"
path <- args[1]
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

borders <- dat %>%
  split(.$week) %>%
  map_df(~hdi( .$cases, 0.95)) %>%
  cbind(week=0:max(dat$week))

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# for mean, mode and median
compute_stat = function(df, f){
  df %>%
    split(.$week) %>%
    map_dbl(~f(.$new)) 
}

# layer data for aggregate statistics
stats = data.frame(means=compute_stat(dat, mean), medians=compute_stat(dat, median), modes=compute_stat(dat, mode))
print(head(stats))
print(str(stats))

# Multiple plot function
# from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
    ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
      layout.pos.col = matchidx$col))
    }
  }
}

p_new <- ggplot(data=dat, aes(x=week, y=cases)) +
  geom_point(alpha=1/100) + #geom_smooth(aes(group=1))
  # geom_line(aes(group=sim_id), alpha=1/10) +
  #stat_summary(geom="point", fun.y=mode, color="magenta") +
  #stat_summary(geom="point", fun.y=mean, color="orange") +
  #stat_summary(geom="point", fun.y=median, color="green") +
  geom_ribbon(data=borders, aes(week, ymin=lo, ymax=hi), fill="blue", alpha=1/10, inherit.aes=FALSE) +
  scale_y_continuous(labels = comma) +
  labs(title="Simulierte Neuerkrankungen ueber 40 Wochen", x="Woche", y="Neuerkrankungen") +
  #annotate("text", color="magenta", label="mode", x=39, y=400000) +
  #annotate("text", color="orange", label="mean", x=39, y=380000) +
  #annotate("text", color="green", label="median", x=39.3, y=360000) + 
  geom_line(data=stats, aes(x=1:nrow(stats),y=means, color="mean"), inherit.aes=FALSE) +
  geom_line(data=stats, aes(x=1:nrow(stats),y=medians, color="median"), inherit.aes=FALSE) +
  geom_line(data=stats, aes(x=1:nrow(stats),y=modes, color="mode"), inherit.aes=FALSE) 

weekly_dists <- ggplot(data=dat, aes(cases)) +
  geom_histogram() +
  facet_wrap(~ week, ncol=7, scales="free") +
  theme(axis.text.x=element_text(size=5)) +
  labs(title="Verteilung der simulierten Neuerkrankungen pro Woche")

print(max(as.numeric(dat$sim_id)))
ggsave(file="season_test.pdf", plot=p_new, height=5, width=10)
ggsave(file="weekly_dists_test.pdf", plot=weekly_dists, height=9, width=11)
