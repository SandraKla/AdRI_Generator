# Age-dependent-Reference-Intervals_Generator 

![](https://img.shields.io/github/license/SandraKla/Age-dependent-Reference-Intervals_Generator.svg)
![](https://img.shields.io/github/last-commit/SandraKla/Age-dependent-Reference-Intervals_Generator/master.svg)
![](https://img.shields.io/github/languages/count/SandraKla/Age-dependent-Reference-Intervals_Generator.svg)
![](https://img.shields.io/github/languages/top/SandraKla/Age-dependent-Reference-Intervals_Generator.svg)

<img src="www/Logo.svg" width="225px" height="150px" align="right"/>

**Shiny App for generating age-dependent analyt-data using functions or given reference intervals!**

This Shiny App is a generator for creating age-dependent analyt-data (for more information see the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/wiki)). The data can be downloaded and used in the Shiny App [**AdRI**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Dataset#adri-generator). 

<img src="www/shiny_generator.png" align="center"/>
<img src="www/shiny_percentile.png" align="center"/>

## Installation 

**Method 1:**
Use the function ```runGitHub()``` from the package [shiny](https://cran.r-project.org/web/packages/shiny/index.html):

```bash
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{install.packages("shiny")}
runGitHub("Age-dependent-Reference-Intervals_Generator", "SandraKla")
```

**Method 2** (not recommended):
Download the Zip-File from this Shiny App. Unzip the file and set your working direction to the path of the folder. 
The package [shiny](https://cran.r-project.org/web/packages/shiny/index.html) (≥ 1.7.1) must be installed before using the Shiny App:

```bash
# Test if shiny is installed:
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{install.packages("shiny")}
```
And then start the app with the following code:
```bash
runApp("app.R")
```

All required packages are downloaded when starting this app or imported if they already exist. For more information about the required packages use the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/wiki).

## Contact

You are welcome to:

- Submit suggestions and Bugs at: https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/issues
- Make a pull request on: https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/pulls

For more information use the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals_Generator/wiki)! 
