# Age-dependent-Reference-Intervals Generator 

<img src="www/Logo.svg" width="225px" height="150px" align="right"/>

**Generator for generating age-dependent data from analytes using functions or given reference intervals!**


This Shiny App is a generator to create age-dependent data from analytes, see the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/wiki). The data can be downloaded and can be used in the Shiny App **AdRI** [Age-dependent-Reference-Intervals](https://github.com/SandraKla/Age-dependent-Reference-Intervals), see also the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Data-from-Generator). 

## Installation

Download the Zip-File from this Shiny App and set the working direction to the order and run:

```bash
# Test if shiny is installed:
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{
  install.packages("shiny")}
```

```bash
library(shiny)
runApp("app.R")
```
Or use the function ```runGitHub()``` from the package *shiny*:

```bash
library(shiny)
runGitHub("Age-dependent-Reference-Intervals_Generator", "SandraKla")
```

All required packages are downloaded when starting this app or read in if they already exist, see also the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/wiki) for the required packages.
