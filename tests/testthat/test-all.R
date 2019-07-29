if ((parallel::detectCores()<2)|(Sys.info()[['sysname']]=='Windows')){
  options(restatapi_cores=1)
}else{
  options(restatapi_cores=2)
}    

clean_restatapi_cache()
context("test of the get_eurostat_toc function")
t1<-system.time({xml_toc<-get_eurostat_toc(verbose=TRUE)})[3]
txt_toc<-get_eurostat_toc(mode="txt",verbose=TRUE)
t2<-system.time({get_eurostat_toc()})[3]
if (!is.null(xml_toc)&!is.null(txt_toc)){
  test_that("test of the get_eurostat_toc function", {
    expect_equal(ncol(xml_toc),14)
    expect_equal(ncol(txt_toc),8)
    expect_equal(nrow(xml_toc),nrow(txt_toc))
    expect_error(get_eurostat_toc(mode="text"))
    expect_true(exists("toc.xml.en", envir = .restatapi_env))
    expect_true(exists("toc.txt.en", envir = .restatapi_env))
    expect_true(t2<t1)
  })
}

context("test of the search_eurostat_toc function")
r1<-search_eurostat_toc("energy",verbose=TRUE)
r2<-search_eurostat_toc("energy",verbose=TRUE,ignore.case=TRUE)
r3<-search_eurostat_toc("energie",lang="de",verbose=TRUE,ignore.case=TRUE)
if (!is.null(r1)&!is.null(r2)&!is.null(r3)){
  test_that("test of the search_eurostat_toc function", {
    expect_true(nrow(r1)<nrow(r2))
    expect_true(nrow(r3)>80)
  })
}

context("test of the get_eurostat_dsd function")
if (!is.null(xml_toc)){
  id<-xml_toc$code[1]
  dsd<-get_eurostat_dsd(id,verbose=TRUE)
  if (!is.null(dsd)){
    test_that("test of the get_eurostat_dsd function", {
      expect_equal(ncol(dsd),3)
      expect_equal(get_eurostat_dsd("text",verbose=TRUE),NULL)
      expect_true(exists(paste0(id,".dsd"), envir = .restatapi_env))
      expect_true(system.time({get_eurostat_dsd(id)})[3]<system.time({get_eurostat_dsd(id,update_cache=TRUE)})[3])
    })
  } 
}

context("test of the search_eurostat_dsd function")
id<-"NAMA_10_GDP"
dsd<-get_eurostat_dsd(id,verbose=TRUE)
eu<-get("cc",envir=.restatapi_env)
pattern<-"EU"
if (!is.null(dsd)){
  test_that("test of the search_eurostat_dsd function", {
    expect_error(search_eurostat_dsd(dsd,pattern))
    expect_equal(search_eurostat_dsd("blabla",dsd),FALSE)
    expect_equal(ncol(search_eurostat_dsd(pattern,dsd)),4)
    expect_equal(nrow(search_eurostat_dsd(pattern,dsd,ignore.case=TRUE)),19)
    expect_equal(nrow(search_eurostat_dsd(pattern,dsd)),15)
    expect_equal(nrow(do.call(rbind,lapply(c(eu$EU15,eu$EA19),search_eurostat_dsd,dsd=dsd,name=FALSE))),41)
  })
}

context("test of the get_eurostat_data function")
id<-"htec_cis3"
t1<-system.time({dt1<-get_eurostat_data(id,keep_flags=TRUE,verbose=TRUE)})[3]
nc1<-ncol(dt1)
t2<-system.time({nc2<-ncol(get_eurostat_data(id,verbose=TRUE))})[3]
if (!is.null(dt1)&is.data.frame(dt1)&!is.null(nc2)){
  test_that("test of the get_eurostat_data function", {
    expect_equal(nrow(dt1),as.numeric(xml_toc$values[xml_toc$code==id]))
    expect_equal(nc2+1,nc1)
    expect_true(t2<t1)
    
  })
}

context("test filtering in the get_eurostat_data function")
test_that("test filtering in the get_eurostat_data function", {
  expect_message(tmp<-get_eurostat_data("agr_r_milkpr",filters="2018"))
  expect_message(get_eurostat_data("agr_r_milkpr",filters="BE",date_filter=22020,keep_flags=TRUE))
  expect_message(get_eurostat_data("agr_r_milkpr",filters="BE",date_filter="<2006<"))
  expect_message(get_eurostat_data("avia_par_me",filters="HU",date_filter="2017-03",select_freq="Q",label=TRUE))
})
tmp<-get_eurostat_data("agr_r_milkpr",filters="2018")
if (!is.null(tmp)&is.data.frame(tmp)){
  test_that("test filtering in the get_eurostat_data function", {
    expect_equal(nrow(tmp),as.numeric(xml_toc$values[xml_toc$code=="agr_r_milkpr"]))
  })
}  
dsd1<-get_eurostat_dsd("agr_r_milkpr")
dsd2<-get_eurostat_dsd("avia_par_me")
if (!is.null(dsd1)&is.data.frame(dsd1)){
  dt3<-get_eurostat_data("agr_r_milkpr",filters="AT",verbose=TRUE)
  nc3<-ncol(dt3)
  nr3<-nrow(dt3)
  dt4<-get_eurostat_data("agr_r_milkpr",filters="AT",exact_match=FALSE,keep_flags=TRUE,verbose=TRUE)
  nc4<-ncol(dt4)
  nr4<-nrow(dt4)
  if (!is.null(dt3)&!is.null(dt4)&is.data.frame(dt3)&is.data.frame(dt4)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nc3+1,nc4)
      expect_true(nr3<nr4)
    })
  }
  nr5<-nrow(get_eurostat_data("agr_r_milkpr",date_filter=2016,verbose=TRUE))
  nr6<-nrow(get_eurostat_data("agr_r_milkpr",date_filter="2016",keep_flags=TRUE,verbose=TRUE))
  if (!is.null(nr5)&!is.null(nr6)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr5,nr6)
    })
  }
  nr7<-nrow(get_eurostat_data("agr_r_milkpr",filters="AT",exact_match=FALSE,keep_flags=TRUE,verbose=TRUE,ignore.case=TRUE))
  nr8<-nrow(get_eurostat_data("agr_r_milkpr",filters="AT",verbose=TRUE))
  if (!is.null(nr7)&!is.null(nr8)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_true(nr7>nr8)
    })
  } 
  nr9<-nrow(get_eurostat_data("agr_r_milkpr",filters="BE$",date_filter="2006-02:2008-06-05",label=TRUE))
  if (!is.null(nr9)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr9,2)
    })
  }
  nr10<-nrow(get_eurostat_data("agr_r_milkpr",filters="BE",date_filter="<2008"))
  if (!is.null(nr10)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr10,14)
    })
  }  
  nr11<-nrow(get_eurostat_data("agr_r_milkpr",filters="^BE$",date_filter=c(2002,"2008",2015:2017),verbose=TRUE))
  if (!is.null(nr11)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_true(nr11<=5)
    })
  }
  nr12<-nrow(get_eurostat_data("agr_r_milkpr",filters="BE",date_filter=c(2008,"2002",2015:2017),verbose=TRUE))
  if (!is.null(nr11)&!is.null(nr12)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr11,nr12)
    })
  }
}
dsd2<-get_eurostat_dsd("avia_par_me")
if (!is.null(dsd2)){
  nr13<-nrow(get_eurostat_data("avia_par_me",filters="BE$",exact_match=FALSE,date_filter=c(2016,"2017-03","2017-05"),select_freq="A",label=TRUE,verbose=FALSE))
  if (!is.null(nr13)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr13,72)
    })
  }
  nr14<-nrow(get_eurostat_data("avia_par_me",date_filter=c(2016,"2017-03","2017-05","2017-07-01"),select_freq="Q"))
  if (!is.null(nr14)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr14,5040)
    })
  }
  nr15<-nrow(get_eurostat_data("avia_par_me",filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2016-08","2017-07-01"),select_freq="M",verbose=TRUE))
  if (!is.null(nr15)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr15,216)
    })
  }  
  dt5<-get_eurostat_data("avia_par_me",filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2016-08","2017-07-01"),select_freq="M",stringsAsFactors=TRUE,verbose=TRUE)
  dt6<-get_eurostat_data("avia_par_me",filters=c("HU","Quarterly","Monthly"),date_filter=c("2016-08","2017-07-01"),stringsAsFactors=FALSE,label=TRUE)
  dt7<-get_eurostat_data("avia_par_me",filters=c("KYIV","BUDAPEST","Quarterly","Monthly"),exact_match=FALSE,date_filter=c("2016-08","2017-07-01"),stringsAsFactors=TRUE,verbose=TRUE)
  if (!is.null(dt5)&!is.null(dt6)&!is.null(dt7)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(dt5,dt7)
      expect_true(any(sapply(dt5,is.factor)))
      expect_false(any(sapply(dt6,is.factor)))
      expect_true(any(sapply(dt7,is.factor)))
    })
  }
}  

context("test of the get_eurostat_raw/bulk function")
id<-"avia_par_me"
raw_txt<-get_eurostat_raw(id,"txt")
raw_xml<-get_eurostat_raw(id,"xml")
if (!is.null(raw_txt)&!is.null(raw_xml)&is.data.frame(raw)){
  test_that("test of the get_eurostat_raw/bulk function", {
    expect_message(bulk<-get_eurostat_bulk(id,verbose=TRUE))
    expect_message(bulk<-get_eurostat_raw(id,mode="text",verbose=TRUE))
    expect_equal(nrow(raw_xml),nrow(raw_txt))
    expect_equal(nrow(raw_txt),as.numeric(xml_toc$values[xml_toc$code==id]))
    expect_true(ncol(raw_xml)>ncol(bulk))
    expect_true(nrow(raw_txt)>nrow(bulk))
  })
}

context("test of the get/put_eurostat_cache function")
clean_restatapi_cache()
id<-"ei_bsfs_q"
xml_toc<-get_eurostat_toc(verbose=TRUE)
udate<-xml_toc$lastUpdate[xml_toc$code==id]
nm<-paste0("r_",id,"-",udate)
rt1<-system.time(raw1<-get_eurostat_raw(id,"xml",keep_flags=TRUE,verbose=TRUE))[3]
rt2<-system.time(raw2<-get_eurostat_raw(id,"xml",cache_dir=tempdir(),verbose=TRUE))[3]
rt3<-system.time(raw3<-get_eurostat_raw(id,"xml",verbose=TRUE))[3]
bt1<-system.time(bulk1<-get_eurostat_bulk(id,stringsAsFactors=FALSE,verbose=TRUE))[3]
nrb1<-nrow(bulk1)
ncb1<-ncol(bulk1)
cnb1<-colnames(bulk1)
dt1<-system.time(estat_data1<-get_eurostat_data(id,keep_flags=TRUE))[3]
dt2<-system.time(estat_data2<-get_eurostat_data(id,stringsAsFactors=FALSE,verbose=TRUE))[3]
nrd2<-nrow(estat_data2)
ncd2<-ncol(estat_data2)
cnd2<-colnames(estat_data2)
dt3<-system.time(estat_data3<-get_eurostat_data(id,keep_flags=TRUE))[3]
id<-"avia_par_mk"
suppressWarnings(dt4<-system.time(estat_data4<-get_eurostat_data(id,stringsAsFactors=FALSE))[3])
rt4<-system.time(raw4<-get_eurostat_raw(id,"xml",keep_flags=TRUE))[3]
suppressWarnings(bt2<-system.time(bulk2<-get_eurostat_bulk(id,keep_flags=TRUE))[3])
if (!is.null(raw1)&is.data.frame(raw1)&!is.null(raw2)&is.data.frame(raw2)&!is.null(raw3)&is.data.frame(raw3)&!is.null(raw4)&is.data.frame(raw4)&!is.null(bulk1)&is.data.frame(bulk1)&!is.null(bulk2)&is.data.frame(bulk2)&!is.null(estat_data1)&is.data.frame(estat_data1)&!is.null(estat_data2)&is.data.frame(estat_data2)&!is.null(estat_data3)&is.data.frame(estat_data3)&!is.null(estat_data4)&is.data.frame(estat_data4)){
  test_that("test of the get/put_eurostat_cache function", {
    expect_true(exists(paste0(nm,"-1"),envir=.restatapi_env))
    expect_true(exists(paste0(nm,"-0"),envir=.restatapi_env))
    expect_true(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0(nm,"-0.rds"))))
    expect_false(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0(nm,"-1.rds"))))
    expect_false(identical(raw1,raw2))
    expect_identical(raw2,raw3)
    expect_identical(bulk1,estat_data2)
    expect_equal(nrb1,nrd2)
    expect_equal(ncb1,ncd2)
    expect_equal(cnb1,cnd2)
    expect_true(rt1>bt1)
    expect_true(rt2<rt1)
    expect_true(rt3<rt1)
#    expect_true(dt1<dt2)
    expect_true(dt3<rt1)
    expect_true(any(sapply(raw1,is.factor)))
    expect_true(any(sapply(raw2,is.factor)))
    expect_true(any(sapply(raw3,is.factor)))
    expect_true(any(sapply(raw4,is.factor)))
    expect_false(any(sapply(bulk1,is.factor)))
    expect_true(any(sapply(bulk2,is.factor)))
    expect_true(any(sapply(estat_data1,is.factor)))
    expect_false(any(sapply(estat_data2,is.factor)))
    expect_true(any(sapply(estat_data3,is.factor)))
    expect_false(any(sapply(estat_data4,is.factor)))
    expect_true(ncol(raw1)>ncol(bulk1))
    expect_equal(ncol(bulk1)+1,ncol(estat_data3))
    expect_equal(nrow(raw1),nrow(bulk1))
    expect_true(bt2<dt4)
    expect_true(bt2<rt4)
    expect_equal(nrow(estat_data4),nrow(bulk2))
    expect_true(nrow(raw4)>nrow(estat_data4))
  })
}

