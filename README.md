# Age-dependent-Reference-Intervals_Generator 

[![](https://img.shields.io/github/license/SandraKla/Age-dependent-Reference-Intervals_Generator.svg)]()
[![](https://img.shields.io/github/last-commit/SandraKla/Age-dependent-Reference-Intervals_Generator/master.svg)]()
[![](https://img.shields.io/github/languages/count/SandraKla/Age-dependent-Reference-Intervals_Generator.svg)]()
[![](https://img.shields.io/github/languages/top/SandraKla/Age-dependent-Reference-Intervals_Generator.svg)]()

<img src="www/Logo.svg" width="225px" height="150px" align="right"/>

**Shiny App for generating age-dependent analyt-data using functions or given reference intervals!**

This Shiny App is a generator for creating age-dependent analyt-data (for more information see the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/wiki)). The data can be downloaded and used in the Shiny App [**AdRI**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Dataset#adri-generator). 

<img src="www/shiny_generator.png" align="center"/>
<img src="www/shiny_percentile.png" align="center"/>

## Installation 

Download the Zip-File from this Shiny App and set your working direction to this path and run:

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

All required packages are downloaded when starting this app or imported if they already exist. For more information about the required packages use the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/wiki).
