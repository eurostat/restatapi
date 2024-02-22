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

if (Sys.info()[['sysname']]=='Windows'){
  options(restatapi_cores=1)
}

if (capabilities("libcurl")){
  options(restatapi_dmethod="libcurl")
}

# options(restatapi_verbose=TRUE)
options(restatapi_log=FALSE)
get("rav",envir=restatapi::.restatapi_env)
clean_restatapi_cache()
not_checked<-NULL

testid1<-"NAMA_10_GDP"
testid2<-"htec_cis3"
testid3<-NULL #an id from the TOC
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


#### CRAN tests
#### test of the get_eurostat_toc function

t1<-system.time({xml_toc<-get_eurostat_toc(verbose=FALSE)})[3]
txt_toc<-get_eurostat_toc(mode="txt")
t2<-system.time({get_eurostat_toc()})[3]
expect_warning(get_eurostat_toc(mode="text")) # 1
if (!is.null(xml_toc)){
  expect_equal(ncol(xml_toc),14) # 2
  expect_true(exists("toc.xml.en",envir=restatapi::.restatapi_env)) # 3
  if (!is.null(txt_toc)){
    expect_equal(ncol(txt_toc),8) # 4
    expect_equal(nrow(xml_toc),nrow(txt_toc)) # 5
    expect_true(exists("toc.txt.en",envir=restatapi::.restatapi_env)) # 6
    expect_true(t2<t1) # 7
  } else {not_checked<-paste(not_checked,"4-7",sep=",")} 
} else {not_checked<-paste(not_checked,"2-7",sep=",")}

#### test of the search_eurostat_toc function
r1<-search_eurostat_toc("energy")
r2<-search_eurostat_toc("energy",ignore.case=TRUE)
r3<-search_eurostat_toc("energie",lang="de",ignore.case=TRUE)
if (!is.null(r1) & !is.null(r2)){
  expect_true(nrow(r1)<nrow(r2)) # 8
} else {not_checked<-paste(not_checked,8,sep=",")}
if (!is.null(r3)){
  expect_true(nrow(r3)/100>0.95) # 9
} else {not_checked<-paste(not_checked,9,sep=",")}

#### test of the get_eurostat_dsd function
expect_equal(suppressMessages(get_eurostat_dsd("text")),NULL) # 10
dsd<-get_eurostat_dsd(testid1)
if (!is.null(dsd)){
  expect_true(data.table::is.data.table(dsd)) # 11
  expect_equal(ncol(dsd),3) # 12
  expect_true(exists(paste0(testid1,".dsd"),envir=restatapi::.restatapi_env)) # 13
} else {not_checked<-paste(not_checked,"11-13",sep=",")} 

#### test of the search_eurostat_dsd function
eu<-get("cc",envir=restatapi::.restatapi_env)
pattern<-"EU"
if (!is.null(xml_toc)){
  if (!is.null(dsd)){
    expect_message(search_eurostat_dsd(dsd,pattern)) # 14
    expect_equal(search_eurostat_dsd("blabla",dsd),NULL) # 15
    expect_equal(ncol(search_eurostat_dsd(pattern,dsd)),4)  # 16
    expect_message(search_eurostat_dsd(eu$NMS2,dsd)) # 17 
  } else {not_checked<-paste(not_checked,"14-17",sep=",")}
} else {not_checked<-paste(not_checked,"14-17",sep=",")}


#### test of the get_eurostat_raw/bulk/data functions
t1<-system.time({dt1<-get_eurostat_data(testid2,keep_flags=FALSE,cflags=TRUE,verbose=FALSE)})[3]
nc1<-ncol(dt1)
t2<-system.time({dt2<-get_eurostat_data(testid2,stringsAsFactors=FALSE)})[3]
nc2<-ncol(dt2)
if (!is.null(dt1)&is.data.frame(dt1)&!is.null(dt2)&is.data.frame(dt2)){
  if (!is.null(xml_toc)){
    if (testid2 %in% xml_toc$code){
      if (!is.na(as.numeric(xml_toc$values[xml_toc$code==testid2]))){
        expect_equal(nrow(dt1),as.numeric(xml_toc$values[xml_toc$code==testid2])) # 18
      } else {not_checked<-paste(not_checked,"18",sep=",")} 
    } else {not_checked<-paste(not_checked,"18",sep=",")}
  } else {not_checked<-paste(not_checked,"18",sep=",")} 
  expect_equal(nc2+1,nc1) # 19
  expect_true(all(is.numeric(dt1$values))) # 20
  expect_true(all(is.numeric(dt2$values)))  # 21
} else {not_checked<-paste(not_checked,"18-21",sep=",")}

if (!is.null(xml_toc)){
  testid3<-xml_toc$code[is.na(xml_toc$values)&is.na(xml_toc$lastUpdate)&is.na(xml_toc$downloadLink.tsv)][1]
  # testid3<-xml_toc$code[(xml_toc$shortDescription=="")&is.na(xml_toc$metadata.html)&is.na(xml_toc$metadata.sdmx)][1]
  if (!is.na(testid3)){
    expect_message(rt1<-get_eurostat_raw(testid3,verbose=FALSE)) # 22
    expect_equal(rt1,NULL) # 23
    expect_message(rt2<-get_eurostat_raw(testid3,check_toc=TRUE,verbose=FALSE)) # 24
    expect_equal(rt2,NULL) # 25
    expect_message(bt1<-get_eurostat_bulk("blabla",check_toc=TRUE,verbose=FALSE)) # 26
    expect_equal(bt1,NULL) # 27
    expect_message(dt3<-get_eurostat_data(testid3,verbose=FALSE)) # 28
    expect_equal(dt3,NULL) # 29
  }
} else {not_checked<-paste(not_checked,"22-29",sep=",")}
rt3<-get_eurostat_raw(testid4,mode="xml",stringsAsFactors=TRUE,keep_flags=TRUE)
bt2<-get_eurostat_data(testid4,keep_flags=TRUE,stringsAsFactors=FALSE)
dt4<-get_eurostat_data(testid4,date_filter=2008,keep_flags=TRUE,stringsAsFactors=FALSE)
if (!is.null(bt2)&!is.null(dt4)){
  expect_true(all.equal(bt2[time==2008,],dt4,check.attributes=FALSE,ignore.row.order=TRUE,ignore.col.order=TRUE)) # 30
} else {not_checked<-paste(not_checked,"30",sep=",")}
if (!is.null(bt2)&!is.null(rt3)){
  expect_true(nrow(rt3)>nrow(bt2)) # 31
} else {not_checked<-paste(not_checked,"31",sep=",")}

rt4<-get_eurostat_raw(testid5,update_cache=TRUE)
bt3<-get_eurostat_bulk(testid5,keep_flags=TRUE)
rt5<-get_eurostat_raw(testid5,mode="xml",stringsAsFactors=TRUE,check_toc=TRUE,keep_flags=TRUE,update_cache=TRUE,verbose=FALSE)
bt4<-get_eurostat_bulk(testid5,keep_flags=TRUE)
if (!is.null(bt3)&!is.null(bt4)){
  kc<-colnames(bt3)[1:(ncol(bt3)-1)]
  data.table::setorderv(bt3,kc)
  data.table::setorderv(bt4,kc)
  expect_true(identical(bt3,bt4)) # 32
} else {not_checked<-paste(not_checked,"32",sep=",")}
if (!is.null(rt4)&!is.null(rt5)){
  expect_true(nrow(rt4)==nrow(rt5)) # 33
  expect_true(ncol(rt4)+2==ncol(rt5)) # 34
} else {not_checked<-paste(not_checked,"33-34",sep=",")}

#### test of filtering in the get_eurostat_data function
expect_message(dt5<-get_eurostat_data(testid4,filters="2018",stringsAsFactors=FALSE))  # 35
expect_message(dt6<-get_eurostat_data(testid4,date_filter=22020)) # 36 - date_filter value used for filters incorrectly => whole dataset downloaded
expect_message(dt7<-get_eurostat_data(testid4,date_filter="<2006<",cache_dir=tempdir(),verbose=FALSE)) # 37
if (!is.null(dt5)){
  expect_false(any(sapply(dt5,is.factor))) # 38
} else {not_checked<-paste(not_checked,"38",sep=",")}
if (!is.null(dt6)){
  expect_true(any(sapply(dt6,is.factor)))  # 39
  if (!is.null(dt7)){
    expect_true(identical(dt6,dt7))  # 40
    expect_true(any(sapply(dt7,is.factor))) # 41
  } else {not_checked<-paste(not_checked,"40-41",sep=",")}
} else {not_checked<-paste(not_checked,"39-41",sep=",")}
expect_message(dt8<-get_eurostat_data(testid6,filters="HU",date_filter="2017-03",select_freq="Q",label=TRUE)) # 42
expect_true(is.null(dt8))  # 43
suppressMessages(dt9<-get_eurostat_data(testid4,filters="2018",cflags=TRUE))
if (!is.null(dt9)&is.data.frame(dt9)&!is.null(xml_toc)){
  if (testid4 %in% xml_toc$code) {
    if (!is.na(as.numeric(xml_toc$values[xml_toc$code==testid4]))){
      expect_equal(nrow(dt9),as.numeric(xml_toc$values[xml_toc$code==testid4])) # 44
    } else {not_checked<-paste(not_checked,"44",sep=",")}  
  } else {not_checked<-paste(not_checked,"44",sep=",")}
} else {not_checked<-paste(not_checked,"44",sep=",")} 
dsd1<-get_eurostat_dsd(testid4)
if (!is.null(dsd1)&is.data.frame(dsd1)){
  dt10<-get_eurostat_data(testid4,filters="AT",verbose=FALSE)
  dt11<-get_eurostat_data(testid4,filters="AT",exact_match=FALSE,keep_flags=TRUE)
  if (!is.null(dt10)&!is.null(dt11)&is.data.frame(dt10)&is.data.frame(dt11)){
    expect_equal(ncol(dt10)+1,ncol(dt11)) # 45
    expect_true(nrow(dt10)<nrow(dt11)) # 46
  } else {not_checked<-paste(not_checked,"45-46",sep=",")}
  nr1<-nrow(get_eurostat_data(testid4,date_filter=2016,mode="xml"))
  nr2<-nrow(get_eurostat_data(testid4,date_filter="2016",keep_flags=TRUE))
  if (!is.null(nr1)&!is.null(nr2)){
    expect_equal(nr1,nr2) # 47
  } else {not_checked<-paste(not_checked,"47",sep=",")}
  nr3<-nrow(get_eurostat_data(testid4,filters="AT",exact_match=FALSE,keep_flags=TRUE,ignore.case=TRUE))
  if (!is.null(nr3)&!is.null(dt10)){
    expect_true(nr3>nrow(dt10)) # 48
  } else {not_checked<-paste(not_checked,"48",sep=",")}
} else {not_checked<-paste(not_checked,"45-48",sep=",")} 

#### test of the get/put_eurostat_cache function
dsd2<-get_eurostat_dsd(testid6)
udate<-format(Sys.Date(),"%Y.%m.%d")
if (!is.null(xml_toc)) {udate2<-xml_toc$lastUpdate[xml_toc$code==testid5]} else {udate2<-NULL}
if (!is.null(rt5)&is.data.frame(rt5)){
  if (!is.null(udate2)){
    expect_true(exists(paste0("r_",testid5,"-",udate2,"-1"),envir=restatapi::.restatapi_env)) # 49
  } else {not_checked<-paste(not_checked,"49",sep=",")}
  expect_true(any(sapply(rt5,is.factor)))  # 50
}  else {not_checked<-paste(not_checked,"49-50",sep=",")}
if (!is.null(rt4)&is.data.frame(rt4)){
  expect_true(exists(paste0("r_",testid5,"-",udate,"-0"),envir=restatapi::.restatapi_env))  # 51
  expect_false(any(sapply(rt4,is.factor)))  # 52
}  else {not_checked<-paste(not_checked,"51-52",sep=",")}
if (!is.null(dsd1)){
  expect_true(exists(paste0(testid4,".dsd"),envir=restatapi::.restatapi_env))  # 53
} else {not_checked<-paste(not_checked,"53",sep=",")}
if (!is.null(dsd2)){
  expect_true(exists(paste0(testid6,".dsd"),envir=restatapi::.restatapi_env))  # 54
} else {not_checked<-paste(not_checked,"54",sep=",")}
expect_false(exists(paste0("b_",testid6,"-",udate,"-0-0-Q"),envir=restatapi::.restatapi_env))   # 55
if (!is.null(rt3)&is.data.frame(rt3)){
  expect_true(exists(paste0("r_",testid4,"-",udate,"-1"),envir=restatapi::.restatapi_env))  # 56
  expect_true(any(sapply(rt3,is.factor)))   # 57
} else {not_checked<-paste(not_checked,"56-57",sep=",")}
expect_false(exists(paste0("r_",testid4,"-",udate,"-0"),envir=restatapi::.restatapi_env))   # 58
if (!is.null(bt2)&is.data.frame(bt2)){
  expect_true(exists(paste0("b_",testid4,"-",udate,"-0-0"),envir=restatapi::.restatapi_env))  # 59
  expect_true(exists(paste0("b_",testid4,"-",udate,"-1-0"),envir=restatapi::.restatapi_env))  # 60
  expect_true(exists(paste0("b_",testid4,"-",udate,"-1-1"),envir=restatapi::.restatapi_env))  # 61
  expect_false(any(sapply(bt2,is.factor)))  # 62
} else {not_checked<-paste(not_checked,"59-62",sep=",")} 
if (!is.null(dt2)&is.data.frame(dt2)){
  expect_true(exists(paste0("b_",testid2,"-",udate,"-0-0"),envir=restatapi::.restatapi_env))  # 63
  expect_false(any(sapply(dt2,is.factor)))  # 64
} else {not_checked<-paste(not_checked,"63-64",sep=",")}
if (!is.null(dt1)&is.data.frame(dt1)){
  expect_true(exists(paste0("b_",testid2,"-",udate,"-1-1"),envir=.restatapi_env))  # 65
  expect_true(any(sapply(dt1,is.factor)))  # 66
} else {not_checked<-paste(not_checked,"65-66",sep=",")}
expect_false(exists(paste0("b_",testid2,"-",udate,"-1-0"),envir=restatapi::.restatapi_env))  # 67
if (!is.null(bt3)&is.data.frame(bt3)){
  expect_true(any(sapply(bt3,is.factor)))  # 68
} else {not_checked<-paste(not_checked,"68",sep=",")}
if (!is.null(dt7)&is.data.frame(dt7)){
  expect_true(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0("b_",testid4,"-",udate,"-0-0.rds"))))  # 69
} else  {not_checked<-paste(not_checked,"69",sep=",")}
expect_false(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0("b_",testid4,"-",udate,"-1-0.rds"))))  # 70
if (!is.null(dt1)&is.data.frame(dt1)){
  expect_false(any(sapply(dt4,is.factor)))  # 71
} else {not_checked<-paste(not_checked,"71",sep=",")}
if (!is.null(dt1)&is.data.frame(dt1)){
  expect_true(any(sapply(bt4,is.factor)))  # 72
} else {not_checked<-paste(not_checked,"72",sep=",")}

#### test of the create_filter_table function
expect_message(dft2<-create_filter_table(c("2017-03","2001-03:2005","<2017-07-01",2012:2014,"2018<",20912,"<3452<",":2018-04>","2<034v","2008:2013","2019-04-32","2019-02-31"),TRUE,verbose=FALSE))  # 73
expect_message(dft3<-create_filter_table(c("2017-03","2001-03:2005","<2017-07-01",2012:2014,"2016<",20912,"<3452<",":2018-04>","2<034v","2008:2013","2019-04-32","2019-02-31"),TRUE,verbose=FALSE))  # 74
expect_message(dft1<-create_filter_table(c("2017-03","2001-03:2005","<2000-07-01",2012:2014,"2018<",20912,"<3452<",":2018-04>","2<034v","2008:2013"),TRUE,verbose=FALSE))  # 75
expect_equal(ncol(dft1),ncol(dft2))  # 76
expect_equal(ncol(dft1),2)  # 77
expect_equal(nrow(dft1),5)  # 78
expect_equal(nrow(dft2),2)  # 79
expect_true(is.null(dft3)) # 80



#### test of the filter_raw_data function
rd<-get_eurostat_raw(testid7)
dsd3<-get_eurostat_dsd(testid7)
if (!is.null(dsd3)&!is.null(rd)){
  ft<-create_filter_table(c("TIME_SP","Hungary",'T'),FALSE,dsd3)
  frd<-filter_raw_data(rd,ft)
  expect_equal(ncol(frd),8) # 81
} else {not_checked<-paste(not_checked,"81",sep=",")}
rd<-get_eurostat_raw(testid6)
if (!is.null(rd)){
  ft<-create_filter_table("2017:2018",TRUE)
  frd<-filter_raw_data(rd,ft,TRUE)
  expect_equal(ncol(frd),6) # 82
} else {not_checked<-paste(not_checked,"82",sep=",")}

#### test of the get_eurostat_codelist function
cl<-get_eurostat_codelist("sex",lang="de",cache=FALSE,verbose=FALSE)
if (!is.null(cl)){
  expect_equal(ncol(cl),2)  # 83
  expect_true(nrow(cl)>0) # 84
} else {not_checked<-paste(not_checked,"83-84",sep=",")}




message("\n",Sys.info()['release'])
message("\n","Are we at home:",at_home())

##################################
# additional test not on CRAN    #
##################################


if (grepl("\\.amzn|-aws|5.4.109+|-azure ",Sys.info()['release'])) {
  
  #### additional test of the get_eurostat_dsd function
  expect_true(system.time({get_eurostat_dsd(testid1)})[3]<system.time({get_eurostat_dsd(testid1,update_cache=TRUE,parallel=FALSE,api_version=api_version)})[3]) # a0
  
  #### additional test of the search_eurostat_dsd function
  expect_equal(nrow(search_eurostat_dsd(pattern,dsd,ignore.case=TRUE)),19) # a1
  expect_equal(nrow(search_eurostat_dsd(pattern,dsd)),15)  # a2
  expect_equal(nrow(do.call(rbind,lapply(c(eu$EU15,eu$EA19),search_eurostat_dsd,dsd=dsd,name=FALSE,exact_match=TRUE))),34) # a3
  expect_equal(nrow(do.call(rbind,lapply(eu$NMS2,search_eurostat_dsd,dsd=dsd,exact_match=TRUE,ignore.case=TRUE))),2) # a4
  
  #### additional test of filtering in the get_eurostat_data function
  nr4<-nrow(get_eurostat_data(testid4,filters="BE$",date_filter="2006-02:2008-06-05",label=TRUE))
  if (!is.null(nr4)){
    expect_equal(nr4,1) # a5
  } else {not_checked<-paste(not_checked,"a5",sep=",")}
  nr5<-nrow(get_eurostat_data(testid4,filters="BE",date_filter="<2008",cflags=TRUE))
  if (!is.null(nr5)){
    expect_equal(nr5,11) # a6
  } else {not_checked<-paste(not_checked,"a6",sep=",")} 
  nr6<-nrow(get_eurostat_data(testid6,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2017-07-01:2017-09-30"),select_freq="M",cflags=TRUE))
  if (!is.null(nr6)){
    expect_equal(nr6,96) # a7
  } else {not_checked<-paste(not_checked,"a7",sep=",")}
  dsd1<-get_eurostat_dsd(testid4)
  if (!is.null(dsd1)&is.data.frame(dsd1)){
    nr7<-nrow(get_eurostat_data(testid4,filters="^BE$",date_filter=c(2002,"2008",2015:2017)))
    if (!is.null(nr7)){
      expect_true(nr7<=5) # a8
      nr8<-nrow(get_eurostat_data(testid4,filters="BE",date_filter=c(2008,"2002",2015:2017)))
      if (!is.null(nr8)){
        expect_equal(nr7,nr8) # a9
      } else {not_checked<-paste(not_checked,"a9",sep=",")}
    } else {not_checked<-paste(not_checked,"a8-a9",sep=",")}
  } else {not_checked<-paste(not_checked,"a8-a9",sep=",")}
  dsd2<-get_eurostat_dsd(testid6)
  if (!is.null(dsd2)){
    nr9<-nrow(get_eurostat_data(testid6,filters="BE$",exact_match=FALSE,date_filter=c(2016,"2017-03","2017-05"),select_freq="A",label=TRUE,cflags=TRUE,verbose=FALSE))
    if (!is.null(nr9)){
      expect_equal(nr9,24) # a10
    } else {not_checked<-paste(not_checked,"a10",sep=",")}
    nr10<-nrow(get_eurostat_data(testid6,date_filter=c(2016,"2017-03","2017-05","2017-07-01:2017-09-30"),select_freq="Q",cflags=TRUE))
    if (!is.null(nr10)){
      expect_equal(nr10,1232) # a11
    } else {not_checked<-paste(not_checked,"a11",sep=",")}
    dt5<-get_eurostat_data(testid6,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=c("2016-08","2017-07-01"),select_freq="M")
    dt6<-get_eurostat_data(testid6,filters=c("HU","Quarterly","Monthly"),date_filter=c("2016-08","2017-07-01"),stringsAsFactors=FALSE,label=TRUE)
    dt7<-get_eurostat_data(testid6,filters=c("ZHULIANY","BUDAPEST","Quarterly","Monthly"),exact_match=FALSE,date_filter=c("2016-08","2017-07-01"),name=TRUE)
    if (!is.null(dt5)){
      expect_true(any(sapply(dt5,is.factor))) # a12
      if (!is.null(dt7)){
        expect_equal(dt5,dt7) # a13
        expect_true(any(sapply(dt7,is.factor))) # a14
      } else {not_checked<-paste(not_checked,"a13-a14",sep=",")}
    } else {not_checked<-paste(not_checked,"a12-a14",sep=",")}
    if (!is.null(dt6)){ 
      expect_false(any(sapply(dt6,is.factor))) # a15
    } else {not_checked<-paste(not_checked,"a15",sep=",")}
    expect_message(dt8<-get_eurostat_data(testid6,filters="BE$",date_filter=c("2017-03",2016,"2017-07-01:2017-09-30",2012:2014),select_freq="Q",label=TRUE,verbose=FALSE,name=FALSE))  # a16 faultcode 150
    if (!is.null(dt8)){
      expect_true(nrow(dt8)<=1232) # a17
      expect_true(ncol(dt8)<=5) # a18
    } else {not_checked<-paste(not_checked,"a17-a18",sep=",")}
    dt9<-get_eurostat_data(testid6,filters="Q...ME_LYPG_HU_LHBP+ME_LYTV_UA_UKKK",date_filter=2017,select_freq="M",cflags=TRUE)
    dt10<-get_eurostat_data(testid6,filters=list(freq="Q",airp_pr=c("ME_LYPG_HU_LHBP","ME_LYTV_UA_UKKK")),date_filter=c("2017"),select_freq="M",cflags=TRUE)
    if (!is.null(dt9)&!is.null(dt10)){
      expect_true(identical(dt9,dt10)) # a19
    } else {not_checked<-paste(not_checked,"a19",sep=",")}
  } else {not_checked<-paste(not_checked,"a10-a19",sep=",")}
  dt11<-get_eurostat_data(testid8,select_freq="Q")
  dt12<-get_eurostat_data(testid8,select_freq="Q")
  if (!is.null(dt11)&!is.null(dt12)){
    expect_true(identical(dt11,dt12)) # a20
  } else {not_checked<-paste(not_checked,"a20",sep=",")}
  dsd3<-get_eurostat_dsd(testid9)
  if (!is.null(dsd3)&is.data.frame(dsd3)){
    nr11<-nrow(get_eurostat_data(testid9,filters="Monthly",exact_match=FALSE,date_filter=c("<2018-07-01"),select_freq="A",label=TRUE,name=FALSE))
    if (!is.null(nr11)){
      expect_equal(nr11,4845) # a21
    } else {not_checked<-paste(not_checked,"a21",sep=",")}
  } else {not_checked<-paste(not_checked,"a21",sep=",")}
  dsd4<-get_eurostat_dsd(testid10)
  if (!is.null(dsd4)&is.data.frame(dsd4)){
    nr12<-nrow(get_eurostat_data(testid10,filters=list(bop_item="SC",currency="MIO_EUR",partner="EXT_EU28",geo=c("EU28","HU"),time="2015:2017",stk_flow="BAL"),date_filter="2010:2012",select_freq="A",label=TRUE,name=FALSE))
    if (!is.null(nr12)){
      expect_equal(nr12,6) # a22
    } else {not_checked<-paste(not_checked,"a22",sep=",")}
  } else {not_checked<-paste(not_checked,"a22",sep=",")}
  yr<-"<2016"
  nr13<-nrow(get_eurostat_data(testid6,date_filter=yr,select_freq="A",verbose=FALSE))
  if (!is.null(nr13)){
    expect_equal(nr13,252) # a23
  } else {not_checked<-paste(not_checked,"a23",sep=",")}
  
  #### additional test of the create_filter_table function
  dsd<-get_eurostat_dsd(testid6) 
  if (!is.null(dsd)){
    ft1<-create_filter_table(c("KYIV","DE","Quarterly"),dsd=dsd,exact_match=FALSE,name=FALSE)
    ft2<-create_filter_table(c("flight","Monthly"),dsd=dsd,exact_match=TRUE,name=TRUE,ignore.case=TRUE)  
    expect_equal(ncol(ft1),ncol(ft2)) # a24
    expect_equal(nrow(ft1),11)  # a25
    expect_equal(nrow(ft2),2)  # a26
    expect_equal(ncol(ft1),4) # a27
  } else {not_checked<-paste(not_checked,"a24-a27",sep=",")}
  
  #### additional test of the filter_raw_data function
  rd<-get_eurostat_raw(testid7)
  dsd<-get_eurostat_dsd(testid7)
  if (!is.null(dsd)&!is.null(rd)){
    ft<-create_filter_table(c("TIME_SP","Hungary",'T'),FALSE,dsd)
    frd<-filter_raw_data(rd,ft)
    expect_equal(nrow(ft),3)  # a28
    expect_equal(nrow(frd),392) # a29
  } else {not_checked<-paste(not_checked,"a28-a29",sep=",")}
  rd<-get_eurostat_raw(testid6)
  if (!is.null(rd)){
    ft<-create_filter_table("2017:2018",TRUE)
    frd<-filter_raw_data(rd,ft,TRUE)
    expect_equal(nrow(ft),1) # a30
    expect_equal(nrow(frd),9316) # a31
  } else {not_checked<-paste(not_checked,"a30-a31",sep=",")}
  
  #### additional test of the get_eurostat_codelist function
  if (!is.null(cl)){
    expect_equal(nrow(cl),7) # a32
  } else {not_checked<-paste(not_checked,"a32",sep=",")}
  
  #### additional test of the get_eurostat_raw/bulk function
  clean_restatapi_cache(tempdir(),verbose=FALSE)
  rt1<-system.time(raw_txt<-get_eurostat_raw(testid6,"txt"))[3]
  raw_xml<-get_eurostat_raw(testid6,"xml")
  raw_unmelted<-get_eurostat_raw(testid6,melt=FALSE)
  rt2<-system.time(raw_txt_check<-get_eurostat_raw(testid6,"txt",check_toc=TRUE))
  expect_message(bulk<-get_eurostat_bulk(testid6))  # a33
  expect_message(raw<-get_eurostat_raw(testid6,mode="text")) # a34
  if (!is.null(raw_txt)&!is.null(raw_xml)&!is.null(raw_unmelted)&!is.null(bulk)){
    if (!is.null(xml_toc)){
      if (!is.na(as.numeric(xml_toc$values[xml_toc$code==testid6]))){
        expect_equal(nrow(raw_txt),as.numeric(xml_toc$values[xml_toc$code==testid6])) # a35
      }  else {not_checked<-paste(not_checked,"a35",sep=",")}
    }  else {not_checked<-paste(not_checked,"a35",sep=",")}
    expect_true(ncol(raw_xml)>ncol(bulk)) # a36
    expect_true(ncol(raw_unmelted)>ncol(raw_xml)) # a37
    expect_false(nrow(raw_unmelted)>nrow(raw_txt)) # a38
    expect_true(ncol(raw_unmelted)==length(unique(raw_txt$time))+1) # a39
    expect_true(ncol(raw_unmelted)>length(unique(bulk$time))) # a40
    expect_true(nrow(raw_txt)>nrow(bulk)) # a41
    expect_equal(nrow(raw_xml),nrow(raw_txt)) # a42
  }  else {not_checked<-paste(not_checked,"a35-a42",sep=",")}
  clean_restatapi_cache()
  raw1<-get_eurostat_raw(testid4,keep_flags=TRUE,update_cache=TRUE,verbose=FALSE)
  bulk1<-get_eurostat_bulk(testid4,keep_flags=TRUE,verbose=FALSE)
  raw2<-get_eurostat_raw(testid4,mode="xml",keep_flags=TRUE,stringsAsFactors=FALSE,update_cache=TRUE)
  bulk2<-get_eurostat_bulk(testid4,keep_flags=TRUE)
  if (!is.null(bulk1)&!is.null(bulk2)){
    kc<-colnames(bulk1)
    bulk1<-bulk1[,..kc]
    bulk1<-bulk2[,..kc]
    data.table::setorder(bulk1)
    data.table::setorder(bulk2)
    expect_true(identical(bulk1,bulk2)) # a43
  } else {not_checked<-paste(not_checked,"a43",sep=",")}
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
  # } else {not_checked<-paste(not_checked,"120",sep=",")}
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
    expect_true(identical(bulk1,bulk2)) # a44
  } else {not_checked<-paste(not_checked,"a44",sep=",")}
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
    expect_true(identical(bulk1,bulk2)) # a45
  } else {not_checked<-paste(not_checked,"a45",sep=",")}
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
    expect_true(identical(bulk1,bulk2)) # a46
  } else {not_checked<-paste(not_checked,"a46",sep=",")}
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
    expect_true(identical(bulk1,bulk2)) # a47
  } else {not_checked<-paste(not_checked,"a47",sep=",")}
  bulk3<-get_eurostat_bulk(testid7,keep_flags=TRUE,stringsAsFactors=TRUE)
  bulk4<-get_eurostat_bulk(testid7,stringsAsFactors=FALSE,update_cache=TRUE)
  if (!is.null(bulk3)&!is.null(bulk4)){
    expect_true(all(is.character(bulk3$values))) # a48
    expect_true(all(is.character(bulk4$values))) # a49
  } else {not_checked<-paste(not_checked,"a48-a49",sep=",")}

  #### additional test of the get/put_eurostat_cache function
  clean_restatapi_cache()
  xml_toc<-get_eurostat_toc(verbose=FALSE)
  # if (!is.null(xml_toc)) {udate2<-xml_toc$lastUpdate[xml_toc$code==testid12]} else {udate2<-NULL}
  udate<-format(Sys.Date(),"%Y.%m.%d")
  nm<-paste0("r_", testid12,"-",udate)
  rt1<-system.time(raw1<-get_eurostat_raw(testid12,"xml",keep_flags=TRUE,stringsAsFactors=TRUE,verbose=FALSE))[3]
  rt2<-system.time(raw2<-get_eurostat_raw(testid12,"xml",cache_dir=tempdir(),verbose=FALSE))[3]
  bt1<-system.time(bulk1<-get_eurostat_bulk(testid12,stringsAsFactors=FALSE,verbose=FALSE))[3]
  nrb1<-nrow(bulk1)
  ncb1<-ncol(bulk1)
  cnb1<-colnames(bulk1)
  rt3<-system.time(raw3<-get_eurostat_raw(testid12,"xml",verbose=FALSE))[3]
  dt1<-system.time(estat_data1<-get_eurostat_data(testid12,keep_flags=TRUE,verbose=FALSE))[3]
  dt2<-system.time(estat_data2<-get_eurostat_data(testid12,stringsAsFactors=FALSE,verbose=FALSE))[3]
  dt3<-system.time(estat_data3<-get_eurostat_data(testid12,keep_flags=TRUE,verbose=FALSE))[3]
  dt4<-system.time(suppressMessages(estat_data4<-get_eurostat_data(testid13,stringsAsFactors=FALSE)))[3]
  rt4<-system.time(raw4<-get_eurostat_raw(testid13,"xml",keep_flags=TRUE))[3]
  bt2<-system.time(suppressMessages(bulk2<-get_eurostat_bulk(testid13,keep_flags=TRUE,verbose=FALSE)))[3]
  if (!is.null(raw1)&is.data.frame(raw1)&!is.null(raw2)&is.data.frame(raw2)&!is.null(raw3)&is.data.frame(raw3)&!is.null(raw4)&is.data.frame(raw4)&!is.null(bulk1)&is.data.frame(bulk1)&!is.null(bulk2)&is.data.frame(bulk2)&!is.null(estat_data1)&is.data.frame(estat_data1)&!is.null(estat_data2)&is.data.frame(estat_data2)&!is.null(estat_data3)&is.data.frame(estat_data3)&!is.null(estat_data4)&is.data.frame(estat_data4)){
    kc<-colnames(bulk1)[1:(ncol(bulk1)-1)]
    data.table::setorderv(bulk1,kc)
    data.table::setorderv(estat_data2,kc)
    nrd2<-nrow(estat_data2)
    ncd2<-ncol(estat_data2)
    cnd2<-colnames(estat_data2)
    expect_true(exists(paste0(nm,"-1"),envir=restatapi::.restatapi_env)) # a50
    expect_true(exists(paste0(nm,"-0"),envir=restatapi::.restatapi_env)) # a51
    expect_true(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0(nm,"-0.rds")))) # a52
    expect_false(file.exists(file.path(sub("[\\/]$","",tempdir(),perl=TRUE),paste0(nm,"-1.rds")))) # a53
    expect_false(identical(raw1,raw2)) # a54
    expect_true(identical(raw2,raw3)) # a55
    expect_true(all.equal(bulk1,estat_data2,check.attributes=FALSE)) # a56
    expect_equal(nrb1,nrd2) # a57
    expect_equal(ncb1,ncd2) # a58
    expect_equal(cnb1,cnd2) # a59
    expect_true(rt1>bt1) # a60
    expect_true(rt2<rt1) # a61
    expect_true(rt3<rt1) # a62
    expect_true(dt3<rt1) # a63
    expect_true(any(sapply(raw1,is.factor))) # a64
    expect_false(any(sapply(raw2,is.factor))) # a65
    expect_false(any(sapply(raw3,is.factor))) # a66
    expect_false(any(sapply(raw4,is.factor))) # a67
    expect_false(any(sapply(bulk1,is.factor))) # a68
    expect_true(any(sapply(bulk2,is.factor))) # a69
    expect_true(any(sapply(estat_data1,is.factor))) # a70
    expect_false(any(sapply(estat_data2,is.factor))) # a71
    expect_true(any(sapply(estat_data3,is.factor))) # a72
    expect_false(any(sapply(estat_data4,is.factor))) # a73
    expect_true(ncol(raw1)>ncol(bulk1)) # a74
    expect_equal(ncol(bulk1)+1,ncol(estat_data3)) # a75
    expect_equal(nrow(raw1),nrow(bulk1)) # a76
    expect_true(bt2<dt4) # a77
    expect_true(bt2<rt4) # a78
    expect_equal(nrow(estat_data4),nrow(bulk2)) # a79
    expect_true(nrow(raw4)>nrow(estat_data4)) # a80
  } else {not_checked<-paste(not_checked,"a50-a80",sep=",")}
  
  
}


##################################
# clean up                       #
##################################


clean_restatapi_cache(tempdir(),verbose=FALSE)
# cat(not_checked)
# print(not_checked)
if (!is.null(not_checked)) {message("\n\n\n\n\nThere are skipped tests:",gsub("^,","",not_checked))}
cat("\n\nSkipped tests:",gsub("^,","",not_checked),"\nconfig version:",get("rav",envir=restatapi::.restatapi_env),"\n")
warnings()
