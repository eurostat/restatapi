if ( requireNamespace("tinytest", quietly=TRUE) ){
  home <- length(unclass(packageVersion("restatapi"))[[1]]) == 4
  tinytest::test_package("restatapi", at_home = home)
}