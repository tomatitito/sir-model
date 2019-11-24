# An implementation of the SIR-Model using probabilistic programming

This project implements the SIR-Model, widely used in epidemiology, in the probabilistic programming language Anglican.
## Installation

https://github.com/tomatitito/sir-model

## Usage

It should be possible to call like this: 

    $ java -jar sir-modell-0.1.0-standalone.jar [args]

Because of this, you have to run `(-main)` from within a repl.

## Results

Running the model forward 10000 times without conditioning on data yields the following distribution over possible influenza seasons. The blue-shaded are represents to 95% highest density interval (HDI), 95% of the outcomes that are more likely than the remaining 5%. Note that, while the HDI first becomes wider and then narrower again towards the end of the season, the variance within the outcomes is very high.

![plot_no_dat](https://github.com/tomatitito/diss/blob/master/figures/ch06-season-plot-no-dat-04_09-10_18.png)

Running the model 10000 times conditioning on data from the 2015/16 influenca season yields the following results. 
I have used 24 datapoints representing weekly new incfections. The data are provided by the *Robert-Koch-Institut (RKI)*.

![plot_dat](https://github.com/tomatitito/diss/blob/master/figures/ch06-season-plot-dat-04_09-10_18.png)

## Options
FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
