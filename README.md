# hud-analysis-template
This is the template for analysis projects. Replace this file with a description of your project and details to help anyone reading the code to understand and reproduce your analysis. e.g.:

* Where does the input data come from?
* What does the output mean?
* Are there any warnings or caveats for the user of this data?
* Are there any special steps or dependencies required to get this project working?

If the project is likely to be reused in the future, be kind to the person working on it in the future:

* What tricky thing did you encounter that's likely to trip up the next person?
* What do you think needs to be added to this project?

### Helpful patterns
You will want `hud-govt-nz/hud-keep` to store and retrieve files.

```r
library(devtools)
install_github("hud-govt-nz/hud-keep")

library(hud.keep)
CONTAINER_URL <- "https://dlreportingdataprod.blob.core.windows.net/sandbox"

list_stored("RE", CONTAINER_URL)
store("README.md", "README-blob.md", CONTAINER_URL) # Store
store("R/keeper.R", "README-blob.md", CONTAINER_URL) # Overwrite - won't work, because the hashes don't match
store("R/keeper.R", "README-blob.md", CONTAINER_URL, forced = TRUE) # Overwrite - will work, because of the forced flag
retrieve("README-blob.md", "test-local.R", CONTAINER_URL)
```
### Helpful files

By creating a repository using this template your repo will automatically contain the file 'template.docx'. This is an Office Word document template to use with the RMarkdown knitting function that will knit your *.rmd files into 'beautiful' word documents in the official HUD style. 

You will need to specify the following in your YAML header, at the top of your RMarkdown file:

``` YAML
---
title: "Reproducible Analytical Workflows"
output:
  word_document:
    toc: yes
    reference_docx: template.docx
---
```
