inf4oec
=======

### Installing Packages

run the R Script "packages.R"

Note that the "install.packages" commands have to be run only once.

### Running

Make sure the working directory is the project's root folder. Then execute the following commands:

```R
  library(shiny)
  runApp(".")
```

Now the app is available as a webservice. See output for the address.

Or check out the screencast in the repository to make the app run.

### IMPORTANT
You need LaTeX installed on your system in order to download the report at the end of the app.<br>
The download handler uses the default setting of texi2pdf, which is often PDFLaTeX.