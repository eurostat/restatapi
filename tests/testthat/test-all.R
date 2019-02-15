options(mc.cores=2)
context("test of the get_eurostat_toc function")
test_that("test of the get_eurostat_toc function", {
  expect_equal(ncol(get_eurostat_toc()),14)
  expect_equal(ncol(get_eurostat_toc(mode="txt")),8)
  expect_error(get_eurostat_toc(mode="text"))
  expect_true(exists("toc.xml.en", envir = .restatapi_env))
  expect_true(exists("toc.txt.en", envir = .restatapi_env))
  expect_true(system.time({get_eurostat_toc()})[3]<system.time({get_eurostat_toc(update_cache=T)})[3])
})

context("test of the search_eurostat_toc function")
test_that("test of the search_eurostat_toc function", {
  expect_true(nrow(search_eurostat_toc("energy"))<nrow(search_eurostat_toc("energy",ignore.case=T)))
  expect_true(nrow(search_eurostat_toc("energie",lang="de",ignore.case=T))>80)
})

context("test of the get_eurostat_dsd function")
toc<-get_eurostat_toc()
id<-toc$code[1]
test_that("test of the get_eurostat_dsd function", {
  expect_equal(ncol(get_eurostat_dsd(id)),3)
  expect_equal(get_eurostat_dsd("text"),NULL)
  expect_true(exists(paste0(id,".dsd"), envir = .restatapi_env))
  expect_true(system.time({get_eurostat_dsd(id)})[3]<system.time({get_eurostat_dsd(id,update_cache=T)})[3])
})


context("test of the search_eurostat_dsd function")
id<-"ei_bsfs_q"
dsd<-get_eurostat_dsd(id)
pattern<-"EU"
test_that("test of the search_eurostat_dsd function", {
  expect_error(search_eurostat_dsd(dsd,pattern))
  expect_equal(search_eurostat_dsd("blabla",dsd),F)
  expect_equal(ncol(search_eurostat_dsd(pattern,dsd)),4)
  expect_equal(nrow(search_eurostat_dsd(pattern,dsd)),9)
  expect_equal(nrow(search_eurostat_dsd(pattern,dsd,ignore.case=F)),5)
})


context("test of the get_eurostat_data function")
id<-"ei_bsfs_q"
test_that("test of the get_eurostat_data function", {
  expect_equal(nrow(get_eurostat_data(id)),as.numeric(toc$values[toc$code==id]))
  expect_equal(ncol(get_eurostat_data(id))+1,ncol(get_eurostat_data(id,keep_flags=T)))
  expect_true(system.time({get_eurostat_data(id)})[3]<system.time({get_eurostat_data(id,update_cache=T)})[3])
})


context("test filtering in the get_eurostat_data function")
test_that("test filtering in the get_eurostat_data function", {
  expect_warning(tmp<-get_eurostat_data("agr_r_milkpr",filters="2018"))
  expect_equal(nrow(tmp),as.numeric(toc$values[toc$code=="agr_r_milkpr"]))
  expect_equal(ncol(get_eurostat_data("agr_r_milkpr",filters="AT$"))+1,ncol(get_eurostat_data("agr_r_milkpr",filters="AT$",keep_flags=T)))
  expect_equal(nrow(get_eurostat_data("agr_r_milkpr",date_filter=2016)),nrow(get_eurostat_data("agr_r_milkpr",date_filter="2016",keep_flags=T)))
  expect_true(nrow(get_eurostat_data("agr_r_milkpr",filters="AT",ignore.case=T,keep_flags=T))>nrow(get_eurostat_data("agr_r_milkpr",filters="AT")))
  expect_true(nrow(get_eurostat_data("agr_r_milkpr",filters="BE",keep_flags=T))>nrow(get_eurostat_data("agr_r_milkpr",filters="BE$")))
  expect_warning(get_eurostat_data("agr_r_milkpr",filters="BE",date_filter=22020,keep_flags=T))
  expect_warning(get_eurostat_data("agr_r_milkpr",filters="BE",date_filter="<2006<"))
  expect_warning(get_eurostat_data("avia_par_me",filters="HU",date_filter="2017-03",select_freq="Q",label=T))
  expect_equal(nrow(get_eurostat_data("agr_r_milkpr",filters="BE$",date_filter="2006-02:2008-06-05",label=T)),2)
  expect_equal(nrow(get_eurostat_data("agr_r_milkpr",filters="BE$",date_filter="<2008")),14)
  expect_equal(nrow(get_eurostat_data("agr_r_milkpr",filters="BE$",date_filter=c(2002,"2008",2015:2017))),5)
  expect_equal(nrow(get_eurostat_data("agr_r_milkpr",filters="BE$",date_filter=c(2008,"2002",2015:2017))),nrow(get_eurostat_data("agr_r_milkpr",filters="BE$",date_filter=c(2002,"2008",2015:2017))))
  expect_equal(nrow(get_eurostat_data("avia_par_me",filters="BE$",date_filter=c(2016,"2017-03","2017-05"),select_freq="A",label=T)),72)
  expect_equal(nrow(get_eurostat_data("avia_par_me",date_filter=c(2016,"2017-03","2017-05","2017-07-01"),select_freq="Q")),4860)
  expect_equal(nrow(get_eurostat_data("avia_par_me",filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2016-08","2017-07-01"),select_freq="M")),216)
  expect_equal(get_eurostat_data("avia_par_me",filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2016-08","2017-07-01"),select_freq="M"),get_eurostat_data("avia_par_me",filters=c("HU","Quarterly","Monthly"),date_filter=c("2016-08","2017-07-01")))
})


context("test of the get_eurostat_raw/bulk function")
id<-"avia_par_me"
raw<-get_eurostat_raw(id)
test_that("test of the get_eurostat_raw/bulk function", {
  expect_warning(bulk<-get_eurostat_bulk(id))
  expect_equal(nrow(raw),as.numeric(toc$values[toc$code==id]))
  expect_true(ncol(raw)>ncol(bulk))
  expect_true(nrow(raw)>nrow(bulk))
})


context("test of the get/put_eurostat_cache function")
id<-"ei_bsfs_q"
udate<-toc$lastUpdate[toc$code==id]
nm<-paste0("r_",id,"-",udate)
rt1<-system.time(raw<-get_eurostat_raw(id,keep_flags=T))[3]
bt1<-system.time(bulk<-get_eurostat_bulk(id,stringsAsFactors=F))[3]
rt2<-system.time(raw2<-get_eurostat_raw(id,cache_dir=tempdir()))[3]
dt1<-system.time(estat_data<-get_eurostat_data(id,keep_flags=T))[3]
rt3<-system.time(raw3<-get_eurostat_raw(id))[3]
dt2<-system.time(estat_data2<-get_eurostat_data(id,update_cache=T,stringsAsFactors=F))[3]
dt3<-system.time(estat_data3<-get_eurostat_data(id,keep_flags=T))[3]
id<-"avia_par_mk"
suppressWarnings(dt4<-system.time(estat_data4<-get_eurostat_data(id,stringsAsFactors=F))[3])
rt4<-system.time(raw4<-get_eurostat_raw(id,keep_flags=T))[3]
suppressWarnings(bt2<-system.time(bulk2<-get_eurostat_bulk(id,keep_flags=T))[3])
test_that("test of the get/put_eurostat_cache function", {
  expect_true(exists(paste0(nm,"-1"),envir=.restatapi_env))
  expect_true(exists(paste0(nm,"-0"),envir=.restatapi_env))
  expect_true(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=T),paste0(nm,"-0.rds"))))
  expect_false(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=T),paste0(nm,"-1.rds"))))
  expect_false(identical(raw,raw2))
  expect_identical(raw2,raw3)
  expect_identical(bulk,estat_data2)
  expect_true(rt1>bt1)
  expect_true(rt2<rt1)
  expect_true(dt1<dt2)
  expect_true(dt3<rt1)
  expect_true(dt2>rt3)
  expect_true(any(sapply(raw,is.factor)))
  expect_true(any(sapply(raw2,is.factor)))
  expect_true(any(sapply(raw3,is.factor)))
  expect_true(any(sapply(raw4,is.factor)))
  expect_false(any(sapply(bulk,is.factor)))
  expect_true(any(sapply(bulk2,is.factor)))
  expect_true(any(sapply(estat_data,is.factor)))
  expect_false(any(sapply(estat_data2,is.factor)))
  expect_true(any(sapply(estat_data3,is.factor)))
  expect_false(any(sapply(estat_data4,is.factor)))
  expect_true(ncol(raw)>ncol(bulk))
  expect_equal(ncol(bulk)+1,ncol(estat_data3))
  expect_equal(nrow(raw),nrow(bulk))
  expect_true(bt2<dt4)
  expect_true(bt2<rt4)
  expect_equal(nrow(estat_data4),nrow(bulk2))
  expect_true(nrow(raw4)>nrow(estat_data4))
})
