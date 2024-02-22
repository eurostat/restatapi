pkgs<-c("pkgdown","devtools","rjson","xml2","data.table")
pkgs_to_install<-pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(pkgs_to_install)>0) install.packages(pkgs_to_install)

devtools::install_github("eurostat/restatapi")
cat(getwd())
message(getwd())
pkgdown::build_site()
system("git config --global --add safe.directory /__w/restatapi/restatapi")
