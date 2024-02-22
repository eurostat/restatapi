if (!("pkgdown" %in% rownames(installed.packages()))){install.packages("pkgdown")}
if (!("devtools" %in% rownames(installed.packages()))){install.packages("devtools")}
if (!("data.table" %in% rownames(installed.packages()))){install.packages("data.table")}
if (!("rjson" %in% rownames(installed.packages()))){install.packages("rjson")}
if (!("xml2" %in% rownames(installed.packages()))){install.packages("xml2")}


devtools::install_github("eurostat/restatapi")
cat(getwd())
message(getwd())
pkgdown::build_site()
system("git config --global --add safe.directory /__w/restatapi/restatapi")
