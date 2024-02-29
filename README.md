# RShiny Demo

This is a demo R Shiny app that can be used to get data from T3 via BrAPI to run an analysis.

Currently, it allows the user to:

1) Select a T3 database (from those defined in the [BrAPI R library](https://github.com/TriticeaeToolbox/BrAPI.R))
2) Select a breeding program (from a list of programs fetched from the database)
3) Select a set of phenotyping trials (from a list of trials fetched from the database for the selected breeding program)
4) Download the trait observations from a selected set of phenotyping trials

The app can then be expanded to run an analysis from the set of data fetched from the database via BrAPI.

## Usage

To run the app, make sure you have the shiny library installed:

```R
install.packages("shiny")
```

Then, use the `runApp()` command to start the app:

```R
library(shiny)
runApp("/path/to/RShinyDemo/")
```

This will start the server, which you can access from http://localhost:5334
