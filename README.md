# promiscuity_app

Currently my (Gergő) code is running if you load the app as described below. Anna's code swoul still be run Whithin the Anna folder the usual way, but there could be errors. Iwill fix any rising errors as soon as possible.

## SETUP

-   Clone main to local project via command line or the RSrudio UI

-   **Install packages** with renv - renv is a package manager, is makes it easier to follow package dependencies in R in collaborative projects

`renv::restore()`

-   **Install devtools** Renv does not install devtools so that has to be installed - devtools allows automatic loading of the app, it can also be used for testing

`renv::install("devtools")`

-   **Load the app -** this allows running the package from the R console, and loads the functions like an R package. devtools::load_all() sources all .R code from the R directory and loads all .rdafiles from the data folder

`devtools::load_all()`

-   **Start the app** - this should start the application in the popup window

`promiscuityApp()`

## How to use

-   **Dependencies:** If new R packages need to be used pleas use renv::install() and then renv::snapshot(), then commit the renv.lock file. This allows other users to synchronize packages with renv::restore()

-   **Code structure:**

    -   app.R - the main script, that defined promiscuityApp() function, that starts the app

    -   promiscuityServer - script of the server. all code is written within a moduleServer() function (modularity is not necessary right now, but will be useful, if the app becomes too complicated)

    -   promiscuityUI - script for UI, to allow modularity, inputIDs, and outputIDs have to be defined within the NS() function. eg: instead of `inputID = "slide"` , `inputID = NS(id, "slide")` has to be used

All other scripts contain other non-reactive functions. These mostly work as they would in a normal environment. One difference is that these scripts should not call Library() or require(). Instead any functions that are not from base or shiny should be called like this: `package_name::function_name()`
