# "Petrify" GTFS-realtime data

This package "petrifies" GTFS-realtime data and turns it into GTFS-static data representing service as-delivered for further analysis.

## Danger, Will Robinson

This package should be considered HIGHLY PRELIMINARY and IN DEVELOPMENT. It is essentially certain there are corner cases in most feeds that will produce unexpected/unwanted/downright incorrect output.

## A note on static analysis of realtime data

It is fairly common in the literature for people to do accessibility analysis, etc. using static GTFS, but this makes an unrealistic assumption that travelers have perfect knowledge of what will happen on their day of travel before it happens (for instance, knowing that you will be able to make a connection because the connecting bus will be late). See Liu et al. (2023) for details.

## Installation

To install, run

```r
pak::pkg_install("github::mattwigway/petrifygtfs")
```

## Usage

Load up a day of real time data using the [`gtfsrealtime`](https://projects.indicatrix.org/gtfsrealtime-r) package, and a corresponding static feed using [`gtfstools`](https://ipea.github.io/gtfstools/). Then run

```r
new_feed = petrify(realtime, static)
```

to get a new feed. Run

```r
gtfstools::write_gtfs(new_feed, "file.zip")
```

to write it out for further analysis.

See the [`petrify`](vignettes/articles/petrify.Rmd) vignette for details.

## References

- Liu, L., Porr, A., & Miller, H. (2023). Realizable accessibility: Evaluating the reliability of public transit accessibility using high-resolution real-time data. Journal of Geographical Systems, 25(3), 429–451. <https://doi.org/10.1007/s10109-022-00382-w>

