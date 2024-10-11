# ARCHIVED

This project is no longer maintained and will not receive any further updates. If you plan to continue using it, please be aware that future security issues will not be addressed.

# sensorweb4R

R extension package to integrate sensor data into R using the [52°North Sensor Web Client API](https://wiki.52north.org/bin/view/SensorWeb/SensorWebClientRESTInterface).

## Installation

The sensorweb4R package is not on CRAN yet, so please download and install the package manually. The first option is using the package ``devtools``, which also works well for your own fork or development versions by other contributors.

So, if you don't have ``devtools`` installed:
```r
install.packages("devtools")
```
Continue:
```r
devtools::install_github("52North/sensorweb4R")
```
To also install the vignettes run:
```r
devtools::install_github("52North/sensorweb4R", build_vignettes = TRUE)
```

Alternatively, you can download the source code and install the package from source. For this to work must have both [git](http://git-scm.com/downloads) and R (see documentation [here](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#Rcmd-is-not-found-in-my-PATH_0021) for Windows) on your path. Then run the following commands:

```shell
git clone https://github.com/52North/sensorweb4R
R CMD INSTALL sensorweb4R
```

## Documentation

### Demos

Take a look at the demos to see how to use the package:

```r
library(sensorweb4R)
demo(package = "sensorweb4R")
```

For some of the demo's you will need:

```r
install.packages("maptools")
install.packages("mapdata")
install.packages("rgdal")
```

On Fedora/RedHat/CentOS you will need the following yum packages to be able to install `rgdal`:

```shell
yum install gdal gdal-devel gdal-static proj-devel proj-epsg
```

On Mac OS X you will need GDAL e.g. precompiled from [here](http://www.kyngchaos.com/software/frameworks).
Depending on the version of the OS, you will probably not find a suitable `rgdal` in CRAN. To install manually:
- download "package source" from `http://cran.r-project.org/web/packages/rgdal/index.html`
- from a terminal:
```shell
cd ~/Downloads/
sudo R CMD INSTALL -configure-args'-with-proj-include=/usr/local/lib' rgdal_0.9-1.tar.gz
```

Then restart R or RStudio.


### Vignette

Futher user documentation is in the R help and the vignettes:

```r
vignette(package = "sensorweb4R")
```

### Reference manual

When installed from source or GitHub, run the command [Rd2pdf](http://cran.r-project.org/doc/manuals/R-exts.html#index-R-CMD-Rd2pdf) on the regular command line from the parent directory of the package. This will create the reference manual, which describes all functions in the package:

```
R CMD Rd2pdf sensorweb4R
```

## Development

### Building the documentation

```r
devtools::document()
```

## Contact / Support

Please direct support questions to the 52°North [Sensor Web Lab mailing list](https://list.52north.org/mailman/listinfo/sensorweb) (and read the [guidelines](https://52north.org/discuss/guidelines/) beforehand).

Add an issue/comment on GitHub if you found a bug or want to collaborate on new features.

## License

This R extension package is licensed under [Apache License 2.0](https://www.tldrlegal.com/l/apache2).

Documentation (namely the vignettes) are published under [CC BY 4.0](http://creativecommons.org/licenses/by/4.0/).
