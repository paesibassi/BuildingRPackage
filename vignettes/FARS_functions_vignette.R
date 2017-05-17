## ---- include=FALSE------------------------------------------------------
library(buildRpackage)
datafile <- system.file("extdata", "accident_2013.csv.bz2", package = "buildRpackage")

## ------------------------------------------------------------------------
tabl <- fars_read(datafile)
tabl

## ------------------------------------------------------------------------
aggregated <- fars_summarize_years(c(2013, 2014, 2015), internal = TRUE)
aggregated

## ---- fig.width=3, fig.height=3, ig.align='center', fig.show='hold'------
fars_map_state(1, 2013, internal = TRUE)
fars_map_state(48, 2015, internal = TRUE)

