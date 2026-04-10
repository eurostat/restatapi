pkgs<-c("pkgdown","devtools","rjson","xml2","data.table","pak")
pkgs_to_install<-pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(pkgs_to_install)>0) install.packages(pkgs_to_install)

pak::pak("eurostat/restatapi")
cat(getwd())
cat(Sys.info()['release'])
message(getwd())
print(Sys.info()['release'])
pkgdown::build_site()
system("git config --global --add safe.directory /__w/restatapi/restatapi")
