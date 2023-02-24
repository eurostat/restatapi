library(restatapi)
library(tinytest)

api_version="new"
source="local"
load_cfg(api_version,source)

if (parallel::detectCores()<=2){
  options(restatapi_cores=1)
}else{
  options(restatapi_cores=2)
}    
if (capabilities("libcurl")){
  options(restatapi_dmethod="libcurl")
}

options(restatapi_verbose=TRUE)
options(restatapi_log=FALSE)
get("rav",envir=restatapi::.restatapi_env)
clean_restatapi_cache()
no_check<-NULL

testid1<-"NAMA_10_GDP"
testid2<-"htec_cis3"
testid4<-"agr_r_milkpr" #teszt_id1
testid5<-"irt_h_eurcoe_d"
testid6<-"avia_par_me" #teszt_id2
testid7<-"tus_00age"
testid8<-"avia_par_ee"
testid9<-"avia_par_is"
testid10<-"bop_its6_det"
testid11<-"nrg_pc_206_h"
testid12<-"ei_bsfs_q"
testid13<-"avia_par_mk"



t1<-system.time({xml_toc<-get_eurostat_toc(verbose=TRUE)})[3]
txt_toc<-get_eurostat_toc(mode="txt")
t2<-system.time({get_eurostat_toc()})[3]
message("\n ########--------- 1 test of the get_eurostat_toc function")
expect_error(get_eurostat_toc(mode="text"))
if (!is.null(xml_toc)){
  message("\n ########--------- 2 test of the get_eurostat_toc function")
  expect_equal(ncol(xml_toc),14)
  message("\n ########--------- 3 test of the get_eurostat_toc function")
  expect_true(exists("toc.xml.en",envir=restatapi::.restatapi_env))
  if (!is.null(txt_toc)){
    message("\n ########--------- 4 test of the get_eurostat_toc function")
    expect_equal(ncol(txt_toc),8)
    message("\n ########--------- 5 test of the get_eurostat_toc function")
    expect_equal(nrow(xml_toc),nrow(txt_toc))
    message("\n ########--------- 6 test of the get_eurostat_toc function")
    expect_true(exists("toc.txt.en",envir=restatapi::.restatapi_env))
    message("\n ########--------- 7 test of the get_eurostat_toc function")
    expect_true(t2<t1)
  } else {no_check<-paste(no_check,"4-7",sep=", ")} 
} else {no_check<-paste(no_check,"2-7",sep=", ")}

r1<-search_eurostat_toc("energy")
r2<-search_eurostat_toc("energy",ignore.case=TRUE)
r3<-search_eurostat_toc("energie",lang="de",ignore.case=TRUE)
if (!is.null(r1)&!is.null(r2)){
  message("\n ########--------- 8 test of the search_eurostat_toc function")
  expect_true(nrow(r1)<nrow(r2))
} else {no_check<-paste(no_check,"8",sep=", ")}
if (!is.null(r3)){
  message("\n ########--------- 9 test of the search_eurostat_toc function")
  expect_true(nrow(r3)>80)
} else {no_check<-paste(no_check,"9",sep=", ")}

message("\n ########--------- 10 test of the get_eurostat_dsd function")
expect_equal(get_eurostat_dsd("text"),NULL)

dsd<-get_eurostat_dsd(testid1)
if (!is.null(dsd)){
  message("\n ########--------- 11 test of the get_eurostat_dsd function")
  expect_equal(ncol(dsd),3)
  message("\n ########--------- 12 test of the get_eurostat_dsd function")
  expect_true(exists(paste0(testid1,".dsd"),envir=restatapi::.restatapi_env))
} else {no_check<-paste(no_check,"11-12",sep=", ")} 
if (!is.null(xml_toc)){}

eu<-get("cc",envir=restatapi::.restatapi_env)
pattern<-"EU"
if (!is.null(xml_toc)){
  if (!is.null(dsd)){
    message("\n ########--------- 13 test of the search_eurostat_dsd function")
    expect_message(search_eurostat_dsd(dsd,pattern))
    message("\n ########--------- 14 test of the search_eurostat_dsd function")
    expect_equal(search_eurostat_dsd("blabla",dsd),NULL)
    message("\n ########--------- 15 test of the search_eurostat_dsd function")
    expect_equal(ncol(search_eurostat_dsd(pattern,dsd)),4)
    message("\n ########--------- 16 test of the search_eurostat_dsd function")
    expect_equal(nrow(search_eurostat_dsd(pattern,dsd,ignore.case=TRUE)),19)
    message("\n ########--------- 17 test of the search_eurostat_dsd function")
    expect_equal(nrow(search_eurostat_dsd(pattern,dsd)),15)
    message("\n ########--------- 18 test of the search_eurostat_dsd function")
    expect_equal(nrow(do.call(rbind,lapply(c(eu$EU15,eu$EA19),search_eurostat_dsd,dsd=dsd,name=FALSE,exact_match=TRUE))),34)
    message("\n ########--------- 19 test of the search_eurostat_dsd function")
    expect_message(search_eurostat_dsd(eu$NMS2,dsd))
    message("\n ########--------- 20 test of the search_eurostat_dsd function")
    expect_equal(nrow(do.call(rbind,lapply(eu$NMS2,search_eurostat_dsd,dsd=dsd,exact_match=TRUE,ignore.case=TRUE))),2)
  } else {no_check<-paste(no_check,"13-20",sep=", ")}
} else {no_check<-paste(no_check,"13-20",sep=", ")}
t1<-system.time({dt1<-get_eurostat_data(testid2,keep_flags=FALSE,cflags=TRUE,verbose=TRUE)})[3]
nc1<-ncol(dt1)
t2<-system.time({dt2<-get_eurostat_data(testid2,stringsAsFactors=FALSE)})[3]
nc2<-ncol(dt2)
if (!is.null(dt1)&is.data.frame(dt1)&!is.null(dt2)&is.data.frame(dt2)){
  if (!is.null(xml_toc)){
    if (!is.na(as.numeric(xml_toc$values[xml_toc$code==testid2]))){
      message("\n ########--------- 21 test of the get_eurostat_data function")
      expect_equal(nrow(dt1),as.numeric(xml_toc$values[xml_toc$code==testid2]))
    } else {no_check<-paste(no_check,"21",sep=", ")}
  } else {no_check<-paste(no_check,"21",sep=", ")} 
  message("\n ########--------- 22 test of the get_eurostat_data function")
  expect_equal(nc2+1,nc1)
  message("\n ########--------- 23 test of the get_eurostat_data function")
  expect_true(all(is.numeric(dt1$values)))
  message("\n ########--------- 24 test of the get_eurostat_data function")
  expect_true(all(is.numeric(dt2$values)))
} else {no_check<-paste(no_check,"21-24",sep=", ")}

if (!is.null(xml_toc)){
  testid3<-xml_toc$code[is.na(xml_toc$values)&is.na(xml_toc$lastUpdate)&is.na(xml_toc$downloadLink.tsv)][1]
  if (!is.na(testid3)){
    message("\n ########--------- 25 test of the get_eurostat_raw/bulk/data functions")
    expect_message(rt1<-get_eurostat_raw(testid3,verbose=FALSE))
    message("\n ########--------- 26 test of the get_eurostat_raw/bulk/data functions")
    expect_equal(rt1,NULL)
    message("\n ########--------- 27 test of the get_eurostat_raw/bulk/data functions")
    expect_message(rt2<-get_eurostat_raw(testid3,check_toc=TRUE,verbose=FALSE))
    message("\n ########--------- 28 test of the get_eurostat_raw/bulk/data functions")
    expect_equal(rt2,NULL)
    message("\n ########--------- 29 test of the get_eurostat_raw/bulk/data functions")
    expect_message(bt1<-get_eurostat_bulk("blabla",check_toc=TRUE,verbose=FALSE))
    message("\n ########--------- 30 test of the get_eurostat_raw/bulk/data functions")
    expect_equal(bt1,NULL)
    message("\n ########--------- 31 test of the get_eurostat_raw/bulk/data functions")
    expect_message(dt3<-get_eurostat_data(testid3,verbose=FALSE))
    message("\n ########--------- 32 test of the get_eurostat_raw/bulk/data functions")
    expect_equal(dt3,NULL)  
  }
} else {no_check<-paste(no_check,"25-32",sep=", ")}
rt3<-get_eurostat_raw(testid4,mode="xml",stringsAsFactors=TRUE,keep_flags=TRUE)
bt2<-get_eurostat_data(testid4,keep_flags=TRUE,stringsAsFactors=FALSE)
dt4<-get_eurostat_data(testid4,date_filter=2008,keep_flags=TRUE,stringsAsFactors=FALSE)
if (!is.null(bt2)&!is.null(dt4)){
  message("\n ########--------- 33 test of the get_eurostat_raw/bulk/data functions")
  expect_true(all.equal(bt2[time==2008,],dt4,check.attributes=FALSE,ignore.row.order=TRUE,ignore.col.order=TRUE))
} else {no_check<-paste(no_check,"33",sep=", ")}
rt4<-get_eurostat_raw(testid5,update_cache=TRUE)
bt3<-get_eurostat_bulk(testid5,keep_flags=TRUE)
rt5<-get_eurostat_raw(testid5,mode="xml",stringsAsFactors=TRUE,check_toc=TRUE,keep_flags=TRUE,update_cache=TRUE,verbose=TRUE)
bt4<-get_eurostat_bulk(testid5,keep_flags=TRUE)
if (!is.null(bt3)&!is.null(bt4)){
  kc<-colnames(bt3)[1:(ncol(bt3)-1)]
  data.table::setorderv(bt3,kc)
  data.table::setorderv(bt4,kc)
  message("\n ########--------- 34 test of the get_eurostat_raw/bulk/data functions")
  expect_true(identical(bt3,bt4))
} else {no_check<-paste(no_check,"34",sep=", ")}
if (!is.null(rt4)&!is.null(rt5)){
  message("\n ########--------- 35 test of the get_eurostat_raw/bulk/data functions")
  expect_true(nrow(rt4)==nrow(rt5))
  message("\n ########--------- 36 test of the get_eurostat_raw/bulk/data functions")
  expect_true(ncol(rt4)+2==ncol(rt5))
} else {no_check<-paste(no_check,"35-36",sep=", ")}

message("\n ########--------- 37 test of filtering in the get_eurostat_data function")
expect_message(dt5<-get_eurostat_data(testid4,filters="2018",stringsAsFactors=FALSE)) 
message("\n ########--------- 38 test of filtering in the get_eurostat_data function")
expect_message(dt6<-get_eurostat_data(testid4,date_filter=22020)) # date_filter value used for filters incorrectly => whole dataset downloaded
message("\n ########--------- 39 test of filtering in the get_eurostat_data function")
expect_message(dt7<-get_eurostat_data(testid4,date_filter="<2006<",cache_dir=tempdir(),verbose=TRUE))
if (!is.null(dt5)){
  message("\n ########--------- 40 test of filtering in the get_eurostat_data function")
  expect_false(any(sapply(dt5,is.factor)))
} else {no_check<-paste(no_check,"40",sep=", ")}
if (!is.null(dt6)){
  message("\n ########--------- 41 test of filtering in the get_eurostat_data function")
  expect_true(any(sapply(dt6,is.factor)))
  if (!is.null(dt7)){
    message("\n ########--------- 42 test of filtering in the get_eurostat_data function")
    expect_true(identical(dt6,dt7))
    message("\n ########--------- 43 test of filtering in the get_eurostat_data function")
    expect_true(any(sapply(dt7,is.factor)))
  } else {no_check<-paste(no_check,"42-43",sep=", ")}
} else {no_check<-paste(no_check,"41-43",sep=", ")}
message("\n ########--------- 44 test of filtering in the get_eurostat_data function")
expect_message(dt8<-get_eurostat_data(testid6,filters="HU",date_filter="2017-03",select_freq="Q",label=TRUE))
message("\n ########--------- 45 test of filtering in the get_eurostat_data function")
expect_true(is.null(dt8))
dt9<-get_eurostat_data(testid4,filters="2018",cflags=TRUE)
if (!is.null(dt9)&is.data.frame(dt9)&!is.null(xml_toc)){
  if (!is.na(as.numeric(xml_toc$values[xml_toc$code==testid4]))){
    message("\n ########--------- 46 test of filtering in the get_eurostat_data function")
    expect_equal(nrow(dt9),as.numeric(xml_toc$values[xml_toc$code==testid4]))
  } else {no_check<-paste(no_check,"46",sep=", ")}
} else {no_check<-paste(no_check,"46",sep=", ")} 
dsd1<-get_eurostat_dsd(testid4)
if (!is.null(dsd1)&is.data.frame(dsd1)){
  dt10<-get_eurostat_data(testid4,filters="AT",verbose=TRUE)
  dt11<-get_eurostat_data(testid4,filters="AT",exact_match=FALSE,keep_flags=TRUE)
  if (!is.null(dt10)&!is.null(dt11)&is.data.frame(dt10)&is.data.frame(dt11)){
    message("\n ########--------- 47 test of filtering in the get_eurostat_data function")
    expect_equal(ncol(dt10)+1,ncol(dt11))
    message("\n ########--------- 48 test of filtering in the get_eurostat_data function")
    expect_true(nrow(dt10)<nrow(dt11))
  } else {no_check<-paste(no_check,"47-48",sep=", ")}
  nr1<-nrow(get_eurostat_data(testid4,date_filter=2016))
  nr2<-nrow(get_eurostat_data(testid4,date_filter="2016",keep_flags=TRUE))
  if (!is.null(nr1)&!is.null(nr2)){
    message("\n ########--------- 49 test of filtering in the get_eurostat_data function")
    expect_equal(nr1,nr2)
  } else {no_check<-paste(no_check,"49",sep=", ")}
  nr3<-nrow(get_eurostat_data(testid4,filters="AT",exact_match=FALSE,keep_flags=TRUE,ignore.case=TRUE))
  if (!is.null(nr3)&!is.null(dt10)){
    message("\n ########--------- 50 test of filtering in the get_eurostat_data function")
    expect_true(nr3>nrow(dt10))
  } else {no_check<-paste(no_check,"50",sep=", ")}
  nr4<-nrow(get_eurostat_data(testid4,filters="BE$",date_filter="2006-02:2008-06-05",label=TRUE))
  if (!is.null(nr4)){
    message("\n ########--------- 51 test of filtering in the get_eurostat_data function")
    expect_equal(nr4,1)
  } else {no_check<-paste(no_check,"51",sep=", ")}
  nr5<-nrow(get_eurostat_data(testid4,filters="BE",date_filter="<2008",cflags=TRUE))
  if (!is.null(nr5)){
    message("\n ########--------- 52 test of filtering in the get_eurostat_data function")
    expect_equal(nr5,11)
  } else {no_check<-paste(no_check,"52",sep=", ")} 
  nr6<-nrow(get_eurostat_data(testid6,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2017-07-01"),select_freq="M",cflags=TRUE))
#  if (!is.null(nr6)){
#    message("\n ########--------- 53 test of filtering in the get_eurostat_data function")
#    expect_equal(nr6,48)
#  } else {no_check<-paste(no_check,"53",sep=", ")} 
} else {no_check<-paste(no_check,"47-53",sep=", ")} 

dsd2<-get_eurostat_dsd(testid6)
udate<-format(Sys.Date(),"%Y.%m.%d")
if (!is.null(xml_toc)) {udate2<-xml_toc$lastUpdate[xml_toc$code==testid5]} else {udate2<-NULL}
if (!is.null(rt5)&is.data.frame(rt5)){
  if (!is.null(udate2)){
    message("\n ########--------- 54 test of the get/put_eurostat_cache function")
    expect_true(exists(paste0("r_",testid5,"-",udate2,"-1"),envir=restatapi::.restatapi_env))
  } else {no_check<-paste(no_check,"54",sep=", ")}
  message("\n ########--------- 55 test of the get/put_eurostat_cache function")
  expect_true(any(sapply(rt5,is.factor)))
}  else {no_check<-paste(no_check,"54-55",sep=", ")}
if (!is.null(rt4)&is.data.frame(rt4)){
  message("\n ########--------- 56 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0("r_",testid5,"-",udate,"-0"),envir=restatapi::.restatapi_env))
  message("\n ########--------- 57 test of the get/put_eurostat_cache function")
  expect_false(any(sapply(rt4,is.factor)))
}  else {no_check<-paste(no_check,"56-57",sep=", ")}
if (!is.null(dsd1)){
  message("\n ########--------- 58 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0(testid4,".dsd"),envir=restatapi::.restatapi_env))
} else {no_check<-paste(no_check,"58",sep=", ")}
if (!is.null(dsd2)){
  message("\n ########--------- 59 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0(testid6,".dsd"),envir=restatapi::.restatapi_env))
} else {no_check<-paste(no_check,"59",sep=", ")}
message("\n ########--------- 60 test of the get/put_eurostat_cache function")
expect_false(exists(paste0("b_",testid6,"-",udate,"-0-0-Q"),envir=restatapi::.restatapi_env))
if (!is.null(rt3)&is.data.frame(rt3)){
  message("\n ########--------- 61 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0("r_",testid4,"-",udate,"-1"),envir=restatapi::.restatapi_env))
  message("\n ########--------- 62 test of the get/put_eurostat_cache function")
  expect_true(any(sapply(rt3,is.factor)))
} else {no_check<-paste(no_check,"61-62",sep=", ")}
message("\n ########--------- 63 test of the get/put_eurostat_cache function")
expect_false(exists(paste0("r_",testid4,"-",udate,"-0"),envir=restatapi::.restatapi_env))
if (!is.null(bt2)&is.data.frame(bt2)){
  message("\n ########--------- 64 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0("b_",testid4,"-",udate,"-0-0"),envir=restatapi::.restatapi_env))
  message("\n ########--------- 65 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0("b_",testid4,"-",udate,"-1-0"),envir=restatapi::.restatapi_env))
  message("\n ########--------- 66 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0("b_",testid4,"-",udate,"-1-1"),envir=restatapi::.restatapi_env))
  message("\n ########--------- 67 test of the get/put_eurostat_cache function")
  expect_false(any(sapply(bt2,is.factor)))
} else {no_check<-paste(no_check,"64-67",sep=", ")} 
if (!is.null(dt2)&is.data.frame(dt2)){
  message("\n ########--------- 68 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0("b_",testid2,"-",udate,"-0-0"),envir=restatapi::.restatapi_env))
  message("\n ########--------- 69 test of the get/put_eurostat_cache function")
  expect_false(any(sapply(dt2,is.factor)))
} else {no_check<-paste(no_check,"68-69",sep=", ")}
if (!is.null(dt1)&is.data.frame(dt1)){
  message("\n ########--------- 70 test of the get/put_eurostat_cache function")
  expect_true(exists(paste0("b_",testid2,"-",udate,"-1-1"),envir=.restatapi_env))
  message("\n ########--------- 71 test of the get/put_eurostat_cache function")
  expect_true(any(sapply(dt1,is.factor)))
} else {no_check<-paste(no_check,"70-71",sep=", ")}
message("\n ########--------- 72 test of the get/put_eurostat_cache function")
expect_false(exists(paste0("b_",testid2,"-",udate,"-1-0"),envir=restatapi::.restatapi_env))
if (!is.null(bt3)&is.data.frame(bt3)){
  message("\n ########--------- 73 test of the get/put_eurostat_cache function")
  expect_true(any(sapply(bt3,is.factor)))
} else {no_check<-paste(no_check,"73",sep=", ")}
if (!is.null(dt7)&is.data.frame(dt7)){
  message("\n ########--------- 74 test of the get/put_eurostat_cache function")
  expect_true(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0("b_",testid4,"-",udate,"-0-0.rds"))))
} else  {no_check<-paste(no_check,"74",sep=", ")}
message("\n ########--------- 75 test of the get/put_eurostat_cache function")
expect_false(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0("b_",testid4,"-",udate,"-1-0.rds"))))
if (!is.null(dt1)&is.data.frame(dt1)){
  message("\n ########--------- 76 test of the get/put_eurostat_cache function")
  expect_false(any(sapply(dt4,is.factor)))
} else {no_check<-paste(no_check,"76",sep=", ")}
if (!is.null(dt1)&is.data.frame(dt1)){
  message("\n ########--------- 77 test of the get/put_eurostat_cache function")
  expect_true(any(sapply(bt4,is.factor)))
} else {no_check<-paste(no_check,"77",sep=", ")}

dft2<-create_filter_table(c("2017-03","2001-03:2005","<2017-07-01",2012:2014,"2018<",20912,"<3452<",":2018-04>","2<034v","2008:2013","2019-04-32","2019-02-31"),T,verbose=T)
dft3<-create_filter_table(c("2017-03","2001-03:2005","<2017-07-01",2012:2014,"2016<",20912,"<3452<",":2018-04>","2<034v","2008:2013","2019-04-32","2019-02-31"),T,verbose=T)
message("\n ########--------- 78 test of the create_filter_table function")
expect_message(dft1<-create_filter_table(c("2017-03","2001-03:2005","<2000-07-01",2012:2014,"2018<",20912,"<3452<",":2018-04>","2<034v","2008:2013"),TRUE,verbose=TRUE))
message("\n ########--------- 79 test of the create_filter_table function")
expect_equal(ncol(dft1),ncol(dft2))
message("\n ########--------- 80 test of the create_filter_table function")
expect_equal(ncol(dft1),2)
message("\n ########--------- 81 test of the create_filter_table function")
expect_equal(nrow(dft1),5)
message("\n ########--------- 82 test of the create_filter_table function")
expect_equal(nrow(dft2),2)
message("\n ########--------- 83 test of the create_filter_table function")
expect_true(is.null(dft3))
dsd<-get_eurostat_dsd(testid6) 
if (!is.null(dsd)){
  ft1<-create_filter_table(c("KYIV","DE","Quarterly"),dsd=dsd,exact_match=FALSE,name=FALSE)
  ft2<-create_filter_table(c("flight","Monthly"),dsd=dsd,exact_match=TRUE,name=TRUE,ignore.case=TRUE)  
  message("\n ########--------- 84 test of the create_filter_table function")
  expect_equal(ncol(ft1),4)
  message("\n ########--------- 85 test of the create_filter_table function")
  expect_equal(ncol(ft1),ncol(ft2))
  message("\n ########--------- 86 test of the create_filter_table function")
  expect_equal(nrow(ft1),10)
  message("\n ########--------- 87 test of the create_filter_table function")
  expect_equal(nrow(ft2),2)
} else {no_check<-paste(no_check,"84-87",sep=", ")}

rd<-get_eurostat_raw(testid7)
dsd<-get_eurostat_dsd(testid7)
if (!is.null(dsd)&!is.null(rd)){
  ft<-create_filter_table(c("TIME_SP","Hungary",'T'),FALSE,dsd)
  frd<-filter_raw_data(rd,ft)
  message("\n ########--------- 88 test of the filter_raw_data function")
  expect_equal(ncol(frd),8)
  message("\n ########--------- 89 test of the filter_raw_data function")
  expect_equal(nrow(ft),3)
  message("\n ########--------- 90 test of the filter_raw_data function")
  expect_equal(nrow(frd),392)
} else {no_check<-paste(no_check,"88-90",sep=", ")}
rd<-get_eurostat_raw(testid6)
if (!is.null(rd)){
  ft<-create_filter_table("2017:2018",TRUE)
  frd<-filter_raw_data(rd,ft,TRUE)
  message("\n ########--------- 91 test of the filter_raw_data function")
  expect_equal(ncol(frd),6)
  message("\n ########--------- 92 test of the filter_raw_data function")
  expect_equal(nrow(ft),1)
  message("\n ########--------- 93 test of the filter_raw_data function")
  expect_equal(nrow(frd),9316)
} else {no_check<-paste(no_check,"91-93",sep=", ")}


##################################
# additional test not on CRAN    #
##################################

if (grepl("\\.amzn|-aws|5.4.109+",Sys.info()['release'])) {
  message("\n ########--------- 94 additional test of the get_eurostat_dsd function") 
  expect_true(system.time({get_eurostat_dsd(testid1)})[3]<system.time({get_eurostat_dsd(testid1,update_cache=TRUE,parallel=FALSE,api_version=api_version)})[3])
  
  dsd1<-get_eurostat_dsd(testid4)
  if (!is.null(dsd1)&is.data.frame(dsd1)){
    nr7<-nrow(get_eurostat_data(testid4,filters="^BE$",date_filter=c(2002,"2008",2015:2017)))
    if (!is.null(nr7)){
      message("\n ########--------- 95 additional tests for filtering in the get_eurostat_data function")
      expect_true(nr7<=5)
      nr8<-nrow(get_eurostat_data(testid4,filters="BE",date_filter=c(2008,"2002",2015:2017)))
      if (!is.null(nr8)){
        message("\n ########--------- 96 additional tests for filtering in the get_eurostat_data function")
        expect_equal(nr7,nr8)
      } else {no_check<-paste(no_check,"96",sep=", ")}
    } else {no_check<-paste(no_check,"95-96",sep=", ")}
  } else {no_check<-paste(no_check,"95-96",sep=", ")}
  
  dsd2<-get_eurostat_dsd(testid6)
  if (!is.null(dsd2)){
    nr9<-nrow(get_eurostat_data(testid6,filters="BE$",exact_match=FALSE,date_filter=c(2016,"2017-03","2017-05"),select_freq="A",label=TRUE,cflags=TRUE,verbose=FALSE))
    if (!is.null(nr9)){
      message("\n ########--------- 97 additional tests for filtering in the get_eurostat_data function")
      expect_equal(nr9,24)
    } else {no_check<-paste(no_check,"97",sep=", ")}
    nr10<-nrow(get_eurostat_data(testid6,date_filter=c(2016,"2017-03","2017-05","2017-07-01"),select_freq="Q",cflags=TRUE))
    if (!is.null(nr10)){
    #  message("\n ########--------- 98 additional tests for filtering in the get_eurostat_data function")
    #  expect_equal(nr10,1232)
    } else {no_check<-paste(no_check,"98",sep=", ")}
    dt5<-get_eurostat_data(testid6,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2016-08","2017-07-01"),select_freq="M")
    dt6<-get_eurostat_data(testid6,filters=c("HU","Quarterly","Monthly"),date_filter=c("2016-08","2017-07-01"),stringsAsFactors=FALSE,label=TRUE)
    dt7<-get_eurostat_data(testid6,filters=c("ZHULIANY","BUDAPEST","Quarterly","Monthly"),exact_match=FALSE,date_filter=c("2016-08","2017-07-01"),name=TRUE)
    if (!is.null(dt5)){
      message("\n ########--------- 99 additional tests for filtering in the get_eurostat_data function")
      expect_true(any(sapply(dt5,is.factor)))
      if (!is.null(dt7)){
        message("\n ########--------- 100 additional tests for filtering in the get_eurostat_data function")
        expect_equal(dt5,dt7)
        message("\n ########--------- 101 additional tests for filtering in the get_eurostat_data function")
        expect_true(any(sapply(dt7,is.factor)))
      } else {no_check<-paste(no_check,"100-101",sep=", ")}
    } else {no_check<-paste(no_check,"99-101",sep=", ")}
    if (!is.null(dt6)){ 
      message("\n ########--------- 102 additional tests for filtering in the get_eurostat_data function")
      expect_false(any(sapply(dt6,is.factor)))
    } else {no_check<-paste(no_check,"102",sep=", ")}
    dt8<-get_eurostat_data(testid6,filters="BE$",date_filter=c("2017-03",2016,"2017-07-01",2012:2014),select_freq="Q",label=TRUE,verbose=FALSE,name=FALSE)
    if (!is.null(dt8)){
      # message("\n ########--------- 103 additional tests for filtering in the get_eurostat_data function")
      # expect_true(nrow(dt8)<=5040)
      message("\n ########--------- 104 additional tests for filtering in the get_eurostat_data function")
      expect_true(ncol(dt8)<=5)
    } else {no_check<-paste(no_check,"103-104",sep=", ")}
    dt9<-get_eurostat_data(testid6,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=2017,select_freq="M",cflags=TRUE)
    dt10<-get_eurostat_data(testid6,filters=list(freq="Q",airp_pr=c("ME_LYPG_HU_LHBP","ME_LYTV_UA_UKKK")),date_filter=c("2017"),select_freq="M",cflags=TRUE)
    if (!is.null(dt9)&!is.null(dt10)){
      message("\n ########--------- 105 additional tests for filtering in the get_eurostat_data function")
      expect_true(identical(dt9,dt10))
    } else {no_check<-paste(no_check,"105",sep=", ")}
  } else {no_check<-paste(no_check,"97-105",sep=", ")}
  dt11<-get_eurostat_data(testid8,select_freq="Q")
  dt12<-get_eurostat_data(testid8,select_freq="Q")
  if (!is.null(dt11)&!is.null(dt12)){
    message("\n ########--------- 106 additional tests for filtering in the get_eurostat_data function")
    expect_true(identical(dt11,dt12))
  } else {no_check<-paste(no_check,"106",sep=", ")}
  dsd3<-get_eurostat_dsd(testid9)
  if (!is.null(dsd3)&is.data.frame(dsd3)){
    nr11<-nrow(get_eurostat_data(testid9,filters="Monthly",exact_match=FALSE,date_filter=c("<2018-07-01"),select_freq="A",label=TRUE,name=FALSE))
    if (!is.null(nr11)){
    #  message("\n ########--------- 107 additional tests for filtering in the get_eurostat_data function")
    #  expect_equal(nr11,5565)
    } else {no_check<-paste(no_check,"107",sep=", ")}
  } else {no_check<-paste(no_check,"107",sep=", ")}
  dsd4<-get_eurostat_dsd(testid10)
  if (!is.null(dsd4)&is.data.frame(dsd4)){
    nr12<-nrow(get_eurostat_data(testid10,filters=list(bop_item="SC",currency="MIO_EUR",partner="EXT_EU28",geo=c("EU28","HU"),time="2010:2017",stk_flow="BAL"),date_filter="2010:2012",select_freq="A",label=TRUE,name=FALSE))
    if (!is.null(nr12)){
      message("\n ########--------- 108 additional tests for filtering in the get_eurostat_data function")
      expect_equal(nr12,6)
    } else {no_check<-paste(no_check,"108",sep=", ")}
  } else {no_check<-paste(no_check,"108",sep=", ")}

  clean_restatapi_cache(tempdir(),verbose=TRUE)
  rt1<-system.time(raw_txt<-get_eurostat_raw(testid6,"txt"))[3]
  raw_xml<-get_eurostat_raw(testid6,"xml")
  raw_unmelted<-get_eurostat_raw(testid6,melt=FALSE)
  rt2<-system.time(raw_txt_check<-get_eurostat_raw(testid6,"txt",check_toc=TRUE))
  message("\n ########--------- 109 additional tests for the get_eurostat_raw/bulk function")
  expect_message(bulk<-get_eurostat_bulk(testid6))
  message("\n ########--------- 110 additional tests for the get_eurostat_raw/bulk function")
  expect_message(raw<-get_eurostat_raw(testid6,mode="text"))
  if (!is.null(raw_txt)&!is.null(raw_xml)&!is.null(raw_unmelted)&!is.null(bulk)){
    message("\n ########--------- 111 additional tests for the get_eurostat_raw/bulk function")
    expect_equal(nrow(raw_xml),nrow(raw_txt))
    if (!is.null(xml_toc)){
      if (!is.na(as.numeric(xml_toc$values[xml_toc$code==testid6]))){
        message("\n ########--------- 112 additional tests for the get_eurostat_raw/bulk function")
        expect_equal(nrow(raw_txt),as.numeric(xml_toc$values[xml_toc$code==testid6]))
      }  else {no_check<-paste(no_check,"112",sep=", ")}
    }  else {no_check<-paste(no_check,"112",sep=", ")}
    message("\n ########--------- 113 additional tests for the get_eurostat_raw/bulk function")
    expect_true(ncol(raw_xml)>ncol(bulk))
    message("\n ########--------- 114 additional tests for the get_eurostat_raw/bulk function")
    expect_true(ncol(raw_unmelted)>ncol(raw_xml))
    message("\n ########--------- 115 additional tests for the get_eurostat_raw/bulk function")
    expect_false(nrow(raw_unmelted)>nrow(raw_txt))
    message("\n ########--------- 116 additional tests for the get_eurostat_raw/bulk function")
    expect_true(ncol(raw_unmelted)==length(unique(raw_txt$time))+1)
    message("\n ########--------- 117 additional tests for the get_eurostat_raw/bulk function")
    expect_true(ncol(raw_unmelted)>length(unique(bulk$time)))
    message("\n ########--------- 118 additional tests for the get_eurostat_raw/bulk function")
    expect_true(nrow(raw_txt)>nrow(bulk))
  }  else {no_check<-paste(no_check,"111-118",sep=", ")}
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(testid4,keep_flags=TRUE,update_cache=TRUE,verbose=TRUE)
  bulk1<-get_eurostat_bulk(testid4,keep_flags=TRUE,verbose=TRUE)
  raw2<-get_eurostat_raw(testid4,mode="xml",keep_flags=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(testid4,keep_flags=TRUE)
  if (!is.null(bulk1)&!is.null(bulk2)){
    kc<-colnames(bulk1)
    bulk1<-bulk1[,..kc]
    bulk1<-bulk2[,..kc]
    data.table::setorder(bulk1)
    data.table::setorder(bulk2)
    message("\n ########--------- 119 additional tests for the get_eurostat_raw/bulk function")
    expect_true(identical(bulk1,bulk2))
  } else {no_check<-paste(no_check,"119",sep=", ")}
  # clean_restatapi_cache()
  # raw1<-get_eurostat_raw(testid4,keep_flags=TRUE,update_cache=TRUE)
  # bulk1<-get_eurostat_bulk(testid4,check_toc=TRUE)
  # raw2<-get_eurostat_raw(testid4,mode="xml",keep_flags=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  # bulk2<-get_eurostat_bulk(testid4)
  # if (!is.null(bulk1)&!is.null(bulk2)){
  #   kc<-colnames(bulk1)
  #   bulk1<-bulk1[,..kc]
  #   bulk1<-bulk2[,..kc]
  #   data.table::setorder(bulk1)
  #   data.table::setorder(bulk2)
  #   message("\n ########--------- 120 additional tests for the get_eurostat_raw/bulk function")
  #   expect_true(identical(bulk1,bulk2))
  # } else {no_check<-paste(no_check,"120",sep=", ")}
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(testid4,keep_flags=TRUE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(testid4,check_toc=TRUE,stringsAsFactors=FALSE)
  raw2<-get_eurostat_raw(testid4,mode="xml",stringsAsFactors=FALSE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(testid4,stringsAsFactors=FALSE)
  if (!is.null(bulk1)&!is.null(bulk2)){
    kc<-colnames(bulk1)
    bulk1<-bulk1[,..kc]
    bulk1<-bulk2[,..kc]
    data.table::setorder(bulk1)
    data.table::setorder(bulk2)
    message("\n ########--------- 121 additional tests for the get_eurostat_raw/bulk function")
    expect_true(identical(bulk1,bulk2))
  } else {no_check<-paste(no_check,"121",sep=", ")}
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(testid4,check_toc=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(testid4,keep_flags=TRUE)
  raw2<-get_eurostat_raw(testid4,mode="xml",check_toc=TRUE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(testid4,check_toc=TRUE,keep_flags=TRUE)
  if (!is.null(bulk1)&!is.null(bulk2)){
    kc<-colnames(bulk1)
    bulk1<-bulk1[,..kc]
    bulk1<-bulk2[,..kc]
    data.table::setorder(bulk1)
    data.table::setorder(bulk2)
    message("\n ########--------- 122 additional tests for the get_eurostat_raw/bulk function")
    expect_true(identical(bulk1,bulk2))
  } else {no_check<-paste(no_check,"122",sep=", ")}
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(testid11,check_toc=TRUE,keep_flags=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(testid11,check_toc=TRUE)
  raw2<-get_eurostat_raw(testid11,mode="xml",check_toc=TRUE,keep_flags=TRUE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(testid11)
  if (!is.null(bulk1)&!is.null(bulk2)){
    kc<-colnames(bulk1)
    bulk1<-bulk1[,..kc]
    bulk1<-bulk2[,..kc]
    data.table::setorder(bulk1)
    data.table::setorder(bulk2)
    message("\n ########--------- 123 additional tests for the get_eurostat_raw/bulk function")
    expect_true(identical(bulk1,bulk2))
  } else {no_check<-paste(no_check,"123",sep=", ")}
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(testid11,check_toc=TRUE,update_cache=TRUE)
  bulk1<-get_eurostat_bulk(testid11,keep_flags=TRUE,check_toc=TRUE,stringsAsFactors=FALSE)
  raw2<-get_eurostat_raw(testid11,mode="xml",keep_flags=TRUE,stringsAsFactors=FALSE,check_toc=TRUE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(testid11,keep_flags=TRUE,stringsAsFactors=FALSE)
  if (!is.null(bulk1)&!is.null(bulk2)){
    kc<-colnames(bulk1)
    bulk1<-bulk1[,..kc]
    bulk1<-bulk2[,..kc]
    data.table::setorder(bulk1)
    data.table::setorder(bulk2)
    message("\n ########--------- 124 additional tests for the get_eurostat_raw/bulk function")
    expect_true(identical(bulk1,bulk2))
  } else {no_check<-paste(no_check,"124",sep=", ")}
  bulk3<-get_eurostat_bulk(testid7,keep_flags=TRUE,stringsAsFactors=TRUE)
  bulk4<-get_eurostat_bulk(testid7,stringsAsFactors=FALSE,update_cache=TRUE)
  if (!is.null(bulk3)&!is.null(bulk4)){
    message("\n ########--------- 125 additional tests for the get_eurostat_raw/bulk function")
    expect_true(all(is.character(bulk3$values)))
    message("\n ########--------- 126 additional tests for the get_eurostat_raw/bulk function")
    expect_true(all(is.character(bulk4$values)))
  } else {no_check<-paste(no_check,"125-126",sep=", ")}

  clean_restatapi_cache()
  xml_toc<-get_eurostat_toc(verbose=TRUE)
  # if (!is.null(xml_toc)) {udate2<-xml_toc$lastUpdate[xml_toc$code==testid12]} else {udate2<-NULL}
  udate<-format(Sys.Date(),"%Y.%m.%d")
  nm<-paste0("r_", testid12,"-",udate)
  rt1<-system.time(raw1<-get_eurostat_raw(testid12,"xml",keep_flags=TRUE,stringsAsFactors=TRUE,verbose=TRUE))[3]
  rt2<-system.time(raw2<-get_eurostat_raw(testid12,"xml",cache_dir=tempdir(),verbose=TRUE))[3]
  bt1<-system.time(bulk1<-get_eurostat_bulk(testid12,stringsAsFactors=FALSE,verbose=TRUE))[3]
  nrb1<-nrow(bulk1)
  ncb1<-ncol(bulk1)
  cnb1<-colnames(bulk1)
  rt3<-system.time(raw3<-get_eurostat_raw(testid12,"xml",verbose=TRUE))[3]
  dt1<-system.time(estat_data1<-get_eurostat_data(testid12,keep_flags=TRUE,verbose=TRUE))[3]
  dt2<-system.time(estat_data2<-get_eurostat_data(testid12,stringsAsFactors=FALSE,verbose=TRUE))[3]
  dt3<-system.time(estat_data3<-get_eurostat_data(testid12,keep_flags=TRUE,verbose=FALSE))[3]
  testid13<-"avia_par_mk"
  suppressWarnings(dt4<-system.time(estat_data4<-get_eurostat_data(testid13,stringsAsFactors=FALSE))[3])
  rt4<-system.time(raw4<-get_eurostat_raw(testid13,"xml",keep_flags=TRUE))[3]
  suppressWarnings(bt2<-system.time(bulk2<-get_eurostat_bulk(testid13,keep_flags=TRUE,verbose=TRUE))[3])
  if (!is.null(raw1)&is.data.frame(raw1)&!is.null(raw2)&is.data.frame(raw2)&!is.null(raw3)&is.data.frame(raw3)&!is.null(raw4)&is.data.frame(raw4)&!is.null(bulk1)&is.data.frame(bulk1)&!is.null(bulk2)&is.data.frame(bulk2)&!is.null(estat_data1)&is.data.frame(estat_data1)&!is.null(estat_data2)&is.data.frame(estat_data2)&!is.null(estat_data3)&is.data.frame(estat_data3)&!is.null(estat_data4)&is.data.frame(estat_data4)){
    kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
    data.table::setorderv(bulk1,kc)
    data.table::setorderv(estat_data2,kc)
    nrd2<-nrow(estat_data2)
    ncd2<-ncol(estat_data2)
    cnd2<-colnames(estat_data2)
    message("\n ########--------- 127 additional tests of the get/put_eurostat_cache function")
    expect_true(exists(paste0(nm,"-1"),envir=restatapi::.restatapi_env))
    message("\n ########--------- 128 additional tests of the get/put_eurostat_cache function")
    expect_true(exists(paste0(nm,"-0"),envir=restatapi::.restatapi_env))
    message("\n ########--------- 129 additional tests of the get/put_eurostat_cache function")
    expect_true(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0(nm,"-0.rds"))))
    message("\n ########--------- 130 additional tests of the get/put_eurostat_cache function")
    expect_false(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0(nm,"-1.rds"))))
    message("\n ########--------- 131 additional tests of the get/put_eurostat_cache function")
    expect_false(identical(raw1,raw2))
    message("\n ########--------- 132 additional tests of the get/put_eurostat_cache function")
    expect_true(identical(raw2,raw3))
    message("\n ########--------- 133 additional tests of the get/put_eurostat_cache function")
    expect_true(all.equal(bulk1,estat_data2,check.attributes=FALSE))
    message("\n ########--------- 134 additional tests of the get/put_eurostat_cache function")
    expect_equal(nrb1,nrd2)
    message("\n ########--------- 135 additional tests of the get/put_eurostat_cache function")
    expect_equal(ncb1,ncd2)
    message("\n ########--------- 136 additional tests of the get/put_eurostat_cache function")
    expect_equal(cnb1,cnd2)
    message("\n ########--------- 137 additional tests of the get/put_eurostat_cache function")
    expect_true(rt1>bt1)
    message("\n ########--------- 138 additional tests of the get/put_eurostat_cache function")
    expect_true(rt2<rt1)
    message("\n ########--------- 139 additional tests of the get/put_eurostat_cache function")
    expect_true(rt3<rt1)
    message("\n ########--------- 140 additional tests of the get/put_eurostat_cache function")
    expect_true(dt3<rt1)
    message("\n ########--------- 141 additional tests of the get/put_eurostat_cache function")
    expect_true(any(sapply(raw1,is.factor)))
    message("\n ########--------- 142 additional tests of the get/put_eurostat_cache function")
    expect_false(any(sapply(raw2,is.factor)))
    message("\n ########--------- 143 additional tests of the get/put_eurostat_cache function")
    expect_false(any(sapply(raw3,is.factor)))
    message("\n ########--------- 144 additional tests of the get/put_eurostat_cache function")
    expect_false(any(sapply(raw4,is.factor)))
    message("\n ########--------- 145 additional tests of the get/put_eurostat_cache function")
    expect_false(any(sapply(bulk1,is.factor)))
    message("\n ########--------- 146 additional tests of the get/put_eurostat_cache function")
    expect_true(any(sapply(bulk2,is.factor)))
    message("\n ########--------- 147 additional tests of the get/put_eurostat_cache function")
    expect_true(any(sapply(estat_data1,is.factor)))
    message("\n ########--------- 148 additional tests of the get/put_eurostat_cache function")
    expect_false(any(sapply(estat_data2,is.factor)))
    message("\n ########--------- 149 additional tests of the get/put_eurostat_cache function")
    expect_true(any(sapply(estat_data3,is.factor)))
    message("\n ########--------- 150 additional tests of the get/put_eurostat_cache function")
    expect_false(any(sapply(estat_data4,is.factor)))
    message("\n ########--------- 151 additional tests of the get/put_eurostat_cache function")
    expect_true(ncol(raw1)>ncol(bulk1))
    message("\n ########--------- 152 additional tests of the get/put_eurostat_cache function")
    expect_equal(ncol(bulk1)+1,ncol(estat_data3))
    message("\n ########--------- 153 additional tests of the get/put_eurostat_cache function")
    expect_equal(nrow(raw1),nrow(bulk1))
    message("\n ########--------- 154 additional tests of the get/put_eurostat_cache function")
    expect_true(bt2<dt4)
    message("\n ########--------- 155 additional tests of the get/put_eurostat_cache function")
    expect_true(bt2<rt4)
    message("\n ########--------- 156 additional tests of the get/put_eurostat_cache function")
    expect_equal(nrow(estat_data4),nrow(bulk2))
    message("\n ########--------- 157 additional tests of the get/put_eurostat_cache function")
    expect_true(nrow(raw4)>nrow(estat_data4))
  } else {no_check<-paste(no_check,"127-157",sep=", ")}
  
  yr<-"<2016"
  nr13<-nrow(get_eurostat_data(testid6,date_filter=yr,select_freq="A",verbose=TRUE))
  if (!is.null(nr13)){
    message("\n ########--------- 158 additional tests for filtering in the get_eurostat_data function")
    expect_equal(nr13,252)
  } else {no_check<-paste(no_check,"158",sep=", ")}
  
}
clean_restatapi_cache(tempdir(),verbose=TRUE)
if (!is.null(no_check)) {message("\n\n\n\n\nThere are skipped tests:",gsub("^,","",no_check))}
cat("\n\nSkipped tests:",gsub("^,","",no_check),"\nconfig version:",get("rav",envir=restatapi::.restatapi_env),"\n")
