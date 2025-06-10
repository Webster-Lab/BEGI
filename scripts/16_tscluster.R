#### read me ####

# the purpose of this script is to use the r package dtwclust to cluster water depth curves by shape to characterize the nature of the variation that is correlated with DO event size

# resources:
# R Journal article: https://journal.r-project.org/articles/RJ-2019-023/
# Exampel paper about turtle dives: https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.4384
# Ethanol example: https://tmastny.github.io/tsrecipes/articles/time-series-clustering.html
# browseVignettes("dtwclust")

#### libraries ####

#library(tsrecipes)
library(tidyverse)
library(dtwclust)


#### load data ####



#### plot data ####

# ethanol %>%
#   mutate(n = list(1:1751)) %>%
#   unnest(c(ts, n)) %>%
#   ggplot(aes(n, ts)) +#, color = as.factor(id))) +
#   geom_line(aes(group = id), alpha = 0.2, show.legend = FALSE) +
#   facet_wrap(~class)
