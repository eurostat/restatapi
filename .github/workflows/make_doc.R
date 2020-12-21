if (!("pkgdown" %in% rownames(installed.packages()))){install.packages("pkgdown")}
cat(getwd())
message(getwd())
pkgdown::build_site()
