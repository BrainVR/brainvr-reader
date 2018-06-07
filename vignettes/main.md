---
title: "Brainvr.R showcase of loading and preprocessing"
author: "Lukáš hejtmy Hejtmánek"
date: "2018-02-07"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Loading single experiemnt
Loading single experimet the first time


```r
library(brainvr.R)
dir_path <- system.file("extdata", package = "brainvr.R")
obj <- load_experiment(dir_path)
#> [1] "Returning only one experiment log."
#> [1] "Loading unprocessed player log_player_.*17-41-52-03-12-2017"
```
#Preprocessing and saving dta

```r
if(!is_player_preprocessed(obj$data$player_log)){
  obj$data$player_log <- preprocess_player_log(obj$data$player_log)
  save_preprocessed_player(dir_path, obj$data$player_log, obj$timestamp)
}
#> [1] "Saving processed player log asC:/Projects/R/brainvr-reader/inst/extdata/NEO_player_17-41-52-03-12-2017_preprocessed.txt"
```

The next time you are loading the log, you will load the processed automatically

Loading processed player log

```r
obj <- load_experiment(dir_path)
#> [1] "Returning only one experiment log."
#> [1] "Loading preprocessed player log_player_.*17-41-52-03-12-2017"
```

# Recentering map
IN case we need a new offset, we can do that as well

```r
obj <- translate_positions(obj, c(33.5, 0, 47.75))
obj <- mirror_axes(obj)
```


# Visualising
Plotting player path

