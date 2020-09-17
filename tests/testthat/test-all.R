if (parallel::detectCores()<=2){
  options(restatapi_cores=1)
}else{
  options(restatapi_cores=2)
}    

options(restatapi_verbose=TRUE)
options(restatapi_log=FALSE)
clean_restatapi_cache()

# suppressWarnings(mem_size<-switch(Sys.info()[['sysname']],
#                                   Windows={tryCatch({as.numeric(gsub("[^0-9]","",system("wmic MemoryChip get Capacity", intern = TRUE)[2]))/1024/1024},error=function(e){0},warning=function(w){0})},
#                                   Darwin={tryCatch({as.numeric(substring(system('hwprefs memory_size', intern = TRUE), 13))},error=function(e){0},warning=function(w){0})},
#                                   SunOS={tryCatch({as.numeric(gsub("[^0-9]","",system("prtconf | grep Memory", intern = TRUE,ignore.stderr=TRUE)))},error=function(e){0},warning=function(w){0})},
#                                   Linux={tryCatch({as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo",intern=TRUE,ignore.stderr=TRUE))/1024},error=function(e){0},warning=function(w){0})}
# ))
# 
# test_that("test mem_size",{
#   expect_true(mem_size>1)
# })

context("test of the get_eurostat_toc function")
t1<-system.time({xml_toc<-get_eurostat_toc(verbose=TRUE)})[3]
txt_toc<-get_eurostat_toc(mode="txt")
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
r1<-search_eurostat_toc("energy")
r2<-search_eurostat_toc("energy",ignore.case=TRUE)
r3<-search_eurostat_toc("energie",lang="de",ignore.case=TRUE)
if (!is.null(r1)&!is.null(r2)&!is.null(r3)){
  test_that("test of the search_eurostat_toc function", {
    expect_true(nrow(r1)<nrow(r2))
    expect_true(nrow(r3)>80)
  })
}

context("test of the get_eurostat_dsd function")
if (!is.null(xml_toc)){
  id<-"NAMA_10_GDP"
  dsd<-get_eurostat_dsd(id)
  if (!is.null(dsd)){
    test_that("test of the get_eurostat_dsd function", {
      expect_equal(ncol(dsd),3)
      expect_equal(get_eurostat_dsd("text"),NULL)
      expect_true(exists(paste0(id,".dsd"), envir = .restatapi_env))
    })
  } 
}

context("test of the search_eurostat_dsd function")
eu<-get("cc",envir=.restatapi_env)
pattern<-"EU"
if (!is.null(dsd)){
  test_that("test of the search_eurostat_dsd function", {
    expect_message(search_eurostat_dsd(dsd,pattern))
    expect_equal(search_eurostat_dsd("blabla",dsd),NULL)
    expect_equal(ncol(search_eurostat_dsd(pattern,dsd)),4)
    expect_equal(nrow(search_eurostat_dsd(pattern,dsd,ignore.case=TRUE)),21)
    expect_equal(nrow(search_eurostat_dsd(pattern,dsd)),17)
    expect_equal(nrow(do.call(rbind,lapply(c(eu$EU15,eu$EA19),search_eurostat_dsd,dsd=dsd,name=FALSE,exact_match=TRUE))),34)
    expect_message(search_eurostat_dsd(eu$NMS2,dsd))
    expect_equal(nrow(do.call(rbind,lapply(eu$NMS2,search_eurostat_dsd,dsd=dsd,exact_match=TRUE,ignore.case=TRUE))),2)
  })
}


context("test of the get_eurostat_data function")
teszt_id0<-"htec_cis3"
t1<-system.time({dt1<-get_eurostat_data(teszt_id0,keep_flags=FALSE,cflags=TRUE)})[3]
nc1<-ncol(dt1)
t2<-system.time({dt2<-get_eurostat_data(teszt_id0,stringsAsFactors=FALSE)})[3]
nc2<-ncol(dt2)
if (!is.null(dt1)&is.data.frame(dt1)&!is.null(dt2)&is.data.frame(dt2)&!is.null(xml_toc)){
  test_that("test of the get_eurostat_data function", {
    expect_equal(nrow(dt1),as.numeric(xml_toc$values[xml_toc$code==teszt_id0]))
    expect_equal(nc2+1,nc1)
    expect_true(all(is.numeric(dt1$values)))
    expect_true(all(is.numeric(dt2$values)))
  })
}
context("test of the get_eurostat_raw/bulk/data functions")
if (!is.null(xml_toc)){
  teszt_id0<-xml_toc$code[is.na(xml_toc$values)&is.na(xml_toc$lastUpdate)][1]
  test_that("test of the get_eurostat_raw/bulk/data functions", {
    expect_message(rt1<-get_eurostat_raw(teszt_id0,verbose=FALSE))
    expect_equal(rt1,NULL)
    expect_message(rt2<-get_eurostat_raw(teszt_id0,check_toc=TRUE,verbose=FALSE))
    expect_equal(rt2,NULL)
    expect_message(bt1<-get_eurostat_bulk("blabla",check_toc=TRUE,verbose=FALSE))
    expect_equal(bt1,NULL)
    expect_message(dt3<-get_eurostat_data(teszt_id0,verbose=FALSE))
    expect_equal(dt3,NULL)
  })
} 
teszt_id1<-"agr_r_milkpr"
rt3<-get_eurostat_raw(teszt_id1,mode="xml",stringsAsFactors=TRUE,keep_flags=TRUE)
bt2<-get_eurostat_data(teszt_id1,keep_flags=TRUE,stringsAsFactors=FALSE)
dt4<-get_eurostat_data(teszt_id1,date_filter=2008,keep_flags=TRUE,stringsAsFactors=FALSE)
if (!is.null(bt2)&!is.null(dt4)){
  test_that("test of the get_eurostat_raw/bulk/data function", {
    expect_true(all.equal(bt2[time==2008,],dt4,check.attributes=FALSE,ignore.row.order=TRUE,ignore.col.order=TRUE))
  })
}
id<-"irt_h_eurcoe_d"
rt4<-get_eurostat_raw(id,update_cache=TRUE)
bt3<-get_eurostat_bulk(id,keep_flags=TRUE)
rt5<-get_eurostat_raw(id,mode="xml",stringsAsFactors=TRUE,check_toc=TRUE,keep_flags=TRUE,update_cache=TRUE)
bt4<-get_eurostat_bulk(id,keep_flags=TRUE)
kc<-colnames(bt3)[1:(ncol(bt3)-1)]
data.table::setorderv(bt3,kc)
data.table::setorderv(bt4,kc)
if (!is.null(bt3)&!is.null(bt4)){
  test_that("test of the get_eurostat_raw/bulk function", {
    expect_true(identical(bt3,bt4))
    expect_true(nrow(rt4)==nrow(rt5))
    expect_true(ncol(rt4)+2==ncol(rt5))
  })
}

teszt_id2<-"avia_par_me"
context("test of filtering in the get_eurostat_data function")
test_that("test of filtering in the get_eurostat_data function", {
  expect_message(dt5<-get_eurostat_data(teszt_id1,filters="2018",stringsAsFactors=FALSE)) # date_filter value used for filters incorrectly => whole dataset downloaded
  expect_message(dt6<-get_eurostat_data(teszt_id1,date_filter=22020))
  expect_message(dt7<-get_eurostat_data(teszt_id1,date_filter="<2006<",cache_dir=tempdir()))
  expect_true(identical(dt6,dt7))
  expect_message(dt8<-get_eurostat_data(teszt_id2,filters="HU",date_filter="2017-03",select_freq="Q",label=TRUE))
  expect_false(any(sapply(dt5,is.factor)))
  expect_true(any(sapply(dt6,is.factor)))
  expect_true(any(sapply(dt7,is.factor)))
  expect_true(is.null(dt8))
})
dt9<-get_eurostat_data(teszt_id1,filters="2018",cflags=TRUE)
if (!is.null(dt9)&is.data.frame(dt9)){
  test_that("test filtering in the get_eurostat_data function", {
    expect_equal(nrow(dt9),as.numeric(xml_toc$values[xml_toc$code=="agr_r_milkpr"]))
  })
}  
dsd1<-get_eurostat_dsd("agr_r_milkpr")
dsd2<-get_eurostat_dsd("avia_par_me")
if (!is.null(dsd1)&is.data.frame(dsd1)){
  dt10<-get_eurostat_data(teszt_id1,filters="AT")
  nc10<-ncol(dt10)
  nr10<-nrow(dt10)
  dt11<-get_eurostat_data(teszt_id1,filters="AT",exact_match=FALSE,keep_flags=TRUE)
  nc11<-ncol(dt11)
  nr11<-nrow(dt11)
  if (!is.null(dt10)&!is.null(dt11)&is.data.frame(dt10)&is.data.frame(dt11)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nc10+1,nc11)
      expect_true(nr10<nr11)
    })
  }
  nr12<-nrow(get_eurostat_data(teszt_id1,date_filter=2016))
  nr13<-nrow(get_eurostat_data(teszt_id1,date_filter="2016",keep_flags=TRUE))
  if (!is.null(nr12)&!is.null(nr13)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr12,nr13)
    })
  }
  nr14<-nrow(get_eurostat_data(teszt_id1,filters="AT",exact_match=FALSE,keep_flags=TRUE,ignore.case=TRUE))
  if (!is.null(nr14)&!is.null(dt10)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_true(nr14>nr10)
    })
  } 
  nr15<-nrow(get_eurostat_data(teszt_id1,filters="BE$",date_filter="2006-02:2008-06-05",label=TRUE))
  if (!is.null(nr15)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr15,2)
    })
  }
  nr16<-nrow(get_eurostat_data(teszt_id1,filters="BE",date_filter="<2008",cflags=TRUE))
  if (!is.null(nr16)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr16,11)
    })
  }  
  nr17<-nrow(get_eurostat_data(teszt_id2,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2017-07-01"),select_freq="M",cflags=TRUE))
  if (!is.null(nr17)){
    test_that("test filtering in the get_eurostat_data function", {
      expect_equal(nr17,48)
    })
  }  
}  

context("test of the get/put_eurostat_cache function")
udate<-format(Sys.Date(),"%Y.%m.%d")
udate2<-xml_toc$lastUpdate[xml_toc$code==id]
if (!is.null(dt1)&is.data.frame(dt1)&!is.null(rt3)&is.data.frame(rt3)&!is.null(rt4)&is.data.frame(rt4)&!is.null(rt5)&is.data.frame(rt5)&!is.null(bt2)&is.data.frame(bt2)&!is.null(bt3)&is.data.frame(bt3)){
  test_that("test of the get/put_eurostat_cache function", {
    expect_false(exists(paste0("b_avia_par_me-",udate,"-0-0-Q"),envir=.restatapi_env))
    expect_true(exists(paste0("r_irt_h_eurcoe_d-",udate,"-0"),envir=.restatapi_env))
    expect_true(exists(paste0("r_irt_h_eurcoe_d-",udate2,"-1"),envir=.restatapi_env))
    expect_true(exists(paste0("b_agr_r_milkpr-",udate,"-0-0"),envir=.restatapi_env))
    expect_true(exists(paste0("r_agr_r_milkpr-",udate,"-1"),envir=.restatapi_env))
    expect_false(exists(paste0("r_agr_r_milkpr-",udate,"-0"),envir=.restatapi_env))
    expect_true(exists(paste0("b_agr_r_milkpr-",udate,"-1-0"),envir=.restatapi_env))
    expect_true(exists(paste0("b_agr_r_milkpr-",udate,"-1-1"),envir=.restatapi_env))
    expect_true(exists(paste0("b_htec_cis3-",udate,"-0-0"),envir=.restatapi_env))
    expect_false(exists(paste0("b_htec_cis3-",udate,"-1-0"),envir=.restatapi_env))
    expect_true(exists(paste0("b_htec_cis3-",udate,"-1-1"),envir=.restatapi_env))
    expect_true(exists("agr_r_milkpr.dsd", envir = .restatapi_env))
    expect_true(exists("avia_par_me.dsd", envir = .restatapi_env))
    expect_true(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0("b_agr_r_milkpr-",udate,"-0-0.rds"))))
    expect_false(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0("b_agr_r_milkpr-",udate,"-1-0.rds"))))
    expect_true(any(sapply(dt1,is.factor)))
    expect_false(any(sapply(dt2,is.factor)))
    expect_false(any(sapply(dt4,is.factor)))
    expect_true(any(sapply(rt3,is.factor)))
    expect_false(any(sapply(rt4,is.factor)))
    expect_true(any(sapply(rt5,is.factor)))
    expect_false(any(sapply(bt2,is.factor)))
    expect_true(any(sapply(bt3,is.factor)))
    expect_true(any(sapply(bt4,is.factor)))
  })
}

context("test of the create_filter_table function")
dft2<-create_filter_table(c("2017-03","2001-03:2005","<2017-07-01",2012:2014,"2018<",20912,"<3452<",":2018-04>","2<034v","2008:2013","2019-04-32","2019-02-31"),T,verbose=T)
dft3<-create_filter_table(c("2017-03","2001-03:2005","<2017-07-01",2012:2014,"2016<",20912,"<3452<",":2018-04>","2<034v","2008:2013","2019-04-32","2019-02-31"),T,verbose=T)
test_that("test of the create_filter_table function", {
  expect_message(dft1<-create_filter_table(c("2017-03","2001-03:2005","<2000-07-01",2012:2014,"2018<",20912,"<3452<",":2018-04>","2<034v","2008:2013"),TRUE,verbose=TRUE))
  expect_equal(ncol(dft1),ncol(dft2))
  expect_equal(ncol(dft1),2)
  expect_equal(nrow(dft1),5)
  expect_equal(nrow(dft2),2)
  expect_true(is.null(dft3))
})
id<-"avia_par_me"
dsd<-get_eurostat_dsd(id) 
if (!is.null(dsd)){
  ft1<-create_filter_table(c("KYIV","DE","Quarterly"),dsd=dsd,exact_match=FALSE,name=FALSE)
  ft2<-create_filter_table(c("flight","Monthly"),dsd=dsd,exact_match=TRUE,name=TRUE,ignore.case=TRUE)  
  test_that("test of the create_filter_table function", {
    expect_equal(ncol(ft1),4)
    expect_equal(ncol(ft1),ncol(ft2))
    expect_equal(nrow(ft1),8)
    expect_equal(nrow(ft2),2)
  })
} 


context("test of the filter_raw_data function")
id<-"tus_00age"
rd<-get_eurostat_raw(id)
dsd<-get_eurostat_dsd(id)
if (!is.null(dsd)&!is.null(rd)){
  ft<-create_filter_table(c("TIME_SP","Hungary",'T'),FALSE,dsd)
  frd<-filter_raw_data(rd,ft)
  test_that("test of the filter_raw_data function", {
    expect_equal(ncol(frd),8)
    expect_equal(nrow(ft),3)
    expect_equal(nrow(frd),392)
  })
}


if (grepl("\\.amzn|-aws",Sys.info()['release'])) {
  id<-"NAMA_10_GDP"
  context("additional test of the get_eurostat_dsd function") 
  test_that("additional test of the get_eurostat_dsd function", {
    expect_true(system.time({get_eurostat_dsd(id)})[3]<system.time({get_eurostat_dsd(id,update_cache=TRUE,parallel=FALSE)})[3])
  })
  context("additional tests for filtering in the get_eurostat_data function")
  
  dsd1<-get_eurostat_dsd("agr_r_milkpr")
  dsd2<-get_eurostat_dsd("avia_par_me")
  if (!is.null(dsd1)&is.data.frame(dsd1)){
    nr11<-nrow(get_eurostat_data(teszt_id1,filters="^BE$",date_filter=c(2002,"2008",2015:2017)))
    if (!is.null(nr11)){
      test_that("test filtering in the get_eurostat_data function", {
        expect_true(nr11<=5)
      })
    }
    nr12<-nrow(get_eurostat_data(teszt_id1,filters="BE",date_filter=c(2008,"2002",2015:2017)))
    if (!is.null(nr11)&!is.null(nr12)){
      test_that("test filtering in the get_eurostat_data function", {
        expect_equal(nr11,nr12)
      })
    }
  }
  if (!is.null(dsd2)){
    nr13<-nrow(get_eurostat_data(teszt_id2,filters="BE$",exact_match=FALSE,date_filter=c(2016,"2017-03","2017-05"),select_freq="A",label=TRUE,cflags=TRUE,verbose=FALSE))
    if (!is.null(nr13)){
      test_that("test filtering in the get_eurostat_data function", {
        expect_equal(nr13,24)
      })
    }
    nr14<-nrow(get_eurostat_data(teszt_id2,date_filter=c(2016,"2017-03","2017-05","2017-07-01"),select_freq="Q",cflags=TRUE))
    if (!is.null(nr14)){
      test_that("test filtering in the get_eurostat_data function", {
        expect_equal(nr14,1152)
      })
    }
    dt5<-get_eurostat_data(teszt_id2,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2016-08","2017-07-01"),select_freq="M")
    dt6<-get_eurostat_data(teszt_id2,filters=c("HU","Quarterly","Monthly"),date_filter=c("2016-08","2017-07-01"),stringsAsFactors=FALSE,label=TRUE)
    dt7<-get_eurostat_data(teszt_id2,filters=c("ZHULIANY","BUDAPEST","Quarterly","Monthly"),exact_match=FALSE,date_filter=c("2016-08","2017-07-01"),name=TRUE)
    if (!is.null(dt5)&!is.null(dt7)&!is.null(dt6)){ #
      test_that("test filtering in the get_eurostat_data function", {
        expect_equal(dt5,dt7)
        expect_true(any(sapply(dt5,is.factor)))
        expect_false(any(sapply(dt6,is.factor)))
        expect_true(any(sapply(dt7,is.factor)))
      })
    }
    dt8<-get_eurostat_data(teszt_id2,filters="BE$",date_filter=c("2017-03",2016,"2017-07-01",2012:2014),select_freq="Q",label=TRUE,verbose=FALSE,name=FALSE)
    if (!is.null(dt8)){
      expect_true(nrow(dt8)<=5040)
      expect_true(ncol(dt8)<=5)
    }
   dt9<-get_eurostat_data(teszt_id2,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=2017,select_freq="M",cflags=TRUE)
   dt10<-get_eurostat_data(teszt_id2,filters=list(freq="Q",airp_pr=c("ME_LYPG_HU_LHBP","ME_LYTV_UA_UKKK")),date_filter=c("2017"),select_freq="M",cflags=TRUE)
   if (!is.null(dt9)&!is.null(dt10)){
     expect_true(identical(dt9,dt10))
   }
   
   teszt_id3<-"avia_par_ee"
   dt11<-get_eurostat_data(teszt_id3,select_freq="Q")
   dt12<-get_eurostat_data(teszt_id3,select_freq="Q")
   if (!is.null(dt11)&!is.null(dt12)){
     expect_true(identical(dt11,dt12))
   }
  } 
  teszt_id4<-"avia_par_is"
  dsd3<-get_eurostat_dsd("avia_par_is")
  if (!is.null(dsd3)&is.data.frame(dsd3)){
   nr16<-nrow(get_eurostat_data(teszt_id4,filters="Monthly",exact_match=FALSE,date_filter=c("<2018-07-01"),select_freq="A",label=TRUE,name=FALSE))
   if (!is.null(nr16)){
     test_that("test filtering in the get_eurostat_data function", {
       expect_equal(nr16,4374)
     })
   }
  }
  teszt_id5<-"bop_its6_det"
  dsd4<-get_eurostat_dsd("bop_its6_det")
  if (!is.null(dsd4)&is.data.frame(dsd4)){
    nr17<-nrow(get_eurostat_data(teszt_id5,filters=list(bop_item="SC",currency="MIO_EUR",partner="EXT_EU28",geo=c("EU28","HU"),time="2010:2017",stk_flow="BAL"),date_filter="2010:2012",select_freq="A",label=TRUE,name=FALSE))
    if (!is.null(nr17)){
      test_that("test filtering in the get_eurostat_data function", {
        expect_equal(nr17,6)
      })
    }
  }
  
  
  
  
  context("additional tests for the get_eurostat_raw/bulk function")
  clean_restatapi_cache()
  id<-"avia_par_me"
  rt1<-system.time(raw_txt<-get_eurostat_raw(id,"txt"))[3]
  raw_xml<-get_eurostat_raw(id,"xml")
  rt2<-system.time(raw_txt_check<-get_eurostat_raw(id,"txt",check_toc=TRUE))
  if (!is.null(raw_txt)&!is.null(raw_xml)&!is.null(raw_txt_check)){
    test_that("test of the get_eurostat_raw/bulk function", {
      expect_message(bulk<-get_eurostat_bulk(id))
      expect_message(raw<-get_eurostat_raw(id,mode="text"))
      expect_equal(nrow(raw_xml),nrow(raw_txt))
      expect_equal(nrow(raw_txt),as.numeric(xml_toc$values[xml_toc$code==id]))
      expect_true(ncol(raw_xml)>ncol(bulk))
      expect_true(nrow(raw_txt)>nrow(bulk))
      expect_true(nrow(raw_txt)>nrow(bulk))
    })
  }
  id<-"avia_par_me"
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(id,keep_flags=TRUE,update_cache=TRUE,verbose=TRUE)
  bulk1<-get_eurostat_bulk(id,keep_flags=TRUE,verbose=TRUE)
  raw2<-get_eurostat_raw(id,mode="xml",keep_flags=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(id,keep_flags=TRUE)
  kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
  data.table::setorderv(bulk1,kc)
  data.table::setorderv(bulk2,kc)
  if (!is.null(bulk1)&!is.null(bulk2)){
    test_that("test of the get_eurostat_raw/bulk function", {
      expect_true(identical(bulk1,bulk2))
    })
  }
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(id,keep_flags=TRUE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(id,check_toc=TRUE)
  raw2<-get_eurostat_raw(id,mode="xml",keep_flags=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(id)
  kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
  data.table::setorderv(bulk1,kc)
  data.table::setorderv(bulk2,kc)
  if (!is.null(bulk1)&!is.null(bulk2)){
    test_that("test of the get_eurostat_raw/bulk function", {
      expect_true(identical(bulk1,bulk2))
    })
  }
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(id,keep_flags=TRUE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(id,check_toc=TRUE,stringsAsFactors=FALSE)
  raw2<-get_eurostat_raw(id,mode="xml",stringsAsFactors=FALSE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(id,stringsAsFactors=FALSE)
  kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
  data.table::setorderv(bulk1,kc)
  data.table::setorderv(bulk2,kc)
  if (!is.null(bulk1)&!is.null(bulk2)){
    test_that("test of the get_eurostat_raw/bulk function", {
      expect_true(identical(bulk1,bulk2))
    })
  }
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(id,check_toc=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(id,keep_flags=TRUE)
  raw2<-get_eurostat_raw(id,mode="xml",check_toc=TRUE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(id,check_toc=TRUE,keep_flags=TRUE)
  kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
  data.table::setorderv(bulk1,kc)
  data.table::setorderv(bulk2,kc)
  if (!is.null(bulk1)&!is.null(bulk2)){
    test_that("test of the get_eurostat_raw/bulk function", {
      expect_true(identical(bulk1,bulk2))
    })
  }
  id<-"nrg_pc_206_h"
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(id,check_toc=TRUE,keep_flags=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(id,check_toc=TRUE)
  raw2<-get_eurostat_raw(id,mode="xml",check_toc=TRUE,keep_flags=TRUE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(id)
  kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
  data.table::setorderv(bulk1,kc)
  data.table::setorderv(bulk2,kc)
  if (!is.null(bulk1)&!is.null(bulk2)){
    test_that("test of the get_eurostat_raw/bulk function", {
      expect_true(identical(bulk1,bulk2))
    })
  }
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(id,check_toc=TRUE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(id,keep_flags=TRUE,check_toc=TRUE,stringsAsFactors=FALSE)
  raw2<-get_eurostat_raw(id,mode="xml",keep_flags=TRUE,stringsAsFactors=FALSE,check_toc=TRUE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(id,keep_flags=TRUE,stringsAsFactors=FALSE)
  kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
  data.table::setorderv(bulk1,kc)
  data.table::setorderv(bulk2,kc)
  if (!is.null(bulk1)&!is.null(bulk2)){
    test_that("test of the get_eurostat_raw/bulk function", {
      expect_true(identical(bulk1,bulk2))
    })
  }
  id<-"tus_00age"
  bulk3<-get_eurostat_bulk(id,keep_flags=TRUE,stringsAsFactors=TRUE)
  bulk4<-get_eurostat_bulk(id,stringsAsFactors=FALSE,update_cache=TRUE)
  if (!is.null(bulk3)&!is.null(bulk4)){
    test_that("test of the get_eurostat_raw/bulk function", {
      expect_true(all(is.character(bulk3$values)))
      expect_true(all(is.character(bulk4$values)))
    })
  }
  
  
  
  context("additional tests of the get/put_eurostat_cache function")
  clean_restatapi_cache()
  teszt_id6<-"ei_bsfs_q"
  xml_toc<-get_eurostat_toc(verbose=TRUE)
  udate2<-xml_toc$lastUpdate[xml_toc$code== teszt_id6]
  udate<-format(Sys.Date(),"%Y.%m.%d")
  nm<-paste0("r_", teszt_id6,"-",udate)
  rt1<-system.time(raw1<-get_eurostat_raw(teszt_id6,"xml",keep_flags=TRUE,stringsAsFactors=TRUE,verbose=TRUE))[3]
  rt2<-system.time(raw2<-get_eurostat_raw(teszt_id6,"xml",cache_dir=tempdir(),verbose=TRUE))[3]
  bt1<-system.time(bulk1<-get_eurostat_bulk(teszt_id6,stringsAsFactors=FALSE,verbose=TRUE))[3]
  nrb1<-nrow(bulk1)
  ncb1<-ncol(bulk1)
  cnb1<-colnames(bulk1)
  rt3<-system.time(raw3<-get_eurostat_raw(teszt_id6,"xml",verbose=TRUE))[3]
  dt1<-system.time(estat_data1<-get_eurostat_data(teszt_id6,keep_flags=TRUE,verbose=TRUE))[3]
  dt2<-system.time(estat_data2<-get_eurostat_data(teszt_id6,stringsAsFactors=FALSE,verbose=TRUE))[3]
  kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
  data.table::setorderv(bulk1,kc)
  data.table::setorderv(estat_data2,kc)
  nrd2<-nrow(estat_data2)
  ncd2<-ncol(estat_data2)
  cnd2<-colnames(estat_data2)
  dt3<-system.time(estat_data3<-get_eurostat_data(teszt_id6,keep_flags=TRUE,verbose=TRUE))[3]
  teszt_id7<-"avia_par_mk"
  suppressWarnings(dt4<-system.time(estat_data4<-get_eurostat_data(teszt_id7,stringsAsFactors=FALSE))[3])
  rt4<-system.time(raw4<-get_eurostat_raw(teszt_id7,"xml",keep_flags=TRUE))[3]
  suppressWarnings(bt2<-system.time(bulk2<-get_eurostat_bulk(teszt_id7,keep_flags=TRUE,verbose=TRUE))[3])
  if (!is.null(raw1)&is.data.frame(raw1)&!is.null(raw2)&is.data.frame(raw2)&!is.null(raw3)&is.data.frame(raw3)&!is.null(raw4)&is.data.frame(raw4)&!is.null(bulk1)&is.data.frame(bulk1)&!is.null(bulk2)&is.data.frame(bulk2)&!is.null(estat_data1)&is.data.frame(estat_data1)&!is.null(estat_data2)&is.data.frame(estat_data2)&!is.null(estat_data3)&is.data.frame(estat_data3)&!is.null(estat_data4)&is.data.frame(estat_data4)){
    test_that("test of the get/put_eurostat_cache function", {
      expect_true(exists(paste0(nm,"-1"),envir=.restatapi_env))
      expect_true(exists(paste0(nm,"-0"),envir=.restatapi_env))
      expect_true(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0(nm,"-0.rds"))))
      expect_false(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0(nm,"-1.rds"))))
      expect_false(identical(raw1,raw2))
      expect_true(identical(raw2,raw3))
      expect_true(all.equal(bulk1,estat_data2,check.attributes=FALSE))
      expect_equal(nrb1,nrd2)
      expect_equal(ncb1,ncd2)
      expect_equal(cnb1,cnd2)
      expect_true(rt1>bt1)
      expect_true(rt2<rt1)
      expect_true(rt3<rt1)
      expect_true(dt3<rt1)
      expect_true(any(sapply(raw1,is.factor)))
      expect_false(any(sapply(raw2,is.factor)))
      expect_false(any(sapply(raw3,is.factor)))
      expect_false(any(sapply(raw4,is.factor)))
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
  
  
}
