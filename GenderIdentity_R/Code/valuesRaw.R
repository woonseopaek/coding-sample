#############################################################################################
# valuesRaw.R
#
# Builds the data file used for empirical analysis. 
#
# Input: MDIS R scripts for the Local Area Labour Force Survey, 2008-2024
#        (in raw/; each script loads its raw data file as `mdis`)
#          2008-2009: <year>_연간자료(A형_시군_중분류)_20260220_37837.R
#          2010-2012: <year>_4분기(A형_시군_중분류)_20260220_19422.R
#          2013-2017: <year>_하반기(C형_시도_중분류_부가항목)_20260220_85933.R
#          2018-2022: <year>_하반기(C형_시도_중분류_부가항목)_20260220_60030.R
#          2023-2024: <year>_하반기(C형_시도_중분류_부가항목)_20260220_35273.R
#
# Output: valuesRaw.csv
#
# Last Updated: Nov 15. 08:00 by Woonseo Paek
#
# Note: The raw data are provided in Korean.
#       Comments in Korean indicate the original variable names to make matching easier. 
##############################################################################################

###housekeeping
dir <- "/Users/paek-woonseo/Desktop/genderidentity2026/USEK_ReplicationFiles"
in_dir <- file.path(dir, "Data", "raw")
out_dir <- file.path(dir, "Data", "derived")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(readr)
library(dplyr)
library(stringr)

###import raw data, rename and select relevant variables

file_list <- list(
  list(year = 2008, r_script = "2008_연간자료(A형_시군_중분류)_20260220_37837.R"),
  list(year = 2009, r_script = "2009_연간자료(A형_시군_중분류)_20260220_37837.R"),
  list(year = 2010, r_script = "2010_4분기(A형_시군_중분류)_20260220_19422.R"),
  list(year = 2011, r_script = "2011_4분기(A형_시군_중분류)_20260220_19422.R"),
  list(year = 2012, r_script = "2012_4분기(A형_시군_중분류)_20260220_19422.R"),
  list(year = 2013, r_script = "2013_하반기(C형_시도_중분류_부가항목)_20260220_85933.R"),
  list(year = 2014, r_script = "2014_하반기(C형_시도_중분류_부가항목)_20260220_85933.R"),
  list(year = 2015, r_script = "2015_하반기(C형_시도_중분류_부가항목)_20260220_85933.R"),
  list(year = 2016, r_script = "2016_하반기(C형_시도_중분류_부가항목)_20260220_85933.R"),
  list(year = 2017, r_script = "2017_하반기(C형_시도_중분류_부가항목)_20260220_85933.R"),
  list(year = 2018, r_script = "2018_하반기(C형_시도_중분류_부가항목)_20260220_60030.R"),
  list(year = 2019, r_script = "2019_하반기(C형_시도_중분류_부가항목)_20260220_60030.R"),
  list(year = 2020, r_script = "2020_하반기(C형_시도_중분류_부가항목)_20260220_60030.R"),
  list(year = 2021, r_script = "2021_하반기(C형_시도_중분류_부가항목)_20260220_60030.R"),
  list(year = 2022, r_script = "2022_하반기(C형_시도_중분류_부가항목)_20260220_60030.R"),
  list(year = 2023, r_script = "2023_하반기(C형_시도_중분류_부가항목)_20260220_35273.R"),
  list(year = 2024, r_script = "2024_하반기(C형_시도_중분류_부가항목)_20260220_35273.R")
)

all_data <- list() 

for (item in file_list) {
  
  cat(paste0(">>> ", item$year, " data processing...\n"))
  
  if (exists("mdis")) rm(mdis)
  
  tryCatch({
    source(file.path(in_dir, item$r_script), local = TRUE, encoding = "UTF-8", chdir = TRUE)
  }, error = function(e) {
    message("encoding error, retrying with CP949")
    source(file.path(in_dir, item$r_script), local = TRUE, encoding = "CP949", chdir = TRUE)
  })
  
  if (!exists("mdis")) {
    stop("cannot find 'mdis'")
  }

  raw <- mdis
 
  if (item$year == 2008) {
    raw <- raw %>%
      rename(
        region = 1, #행정구역       
        rel_head = 2, #가구주와의관계  
        sex = 3, #성별
        age = 4, #만연령
        edu_level = 5, #교육정도(학력)
        marry= 8, #혼인상태
        emp_w1 =10, #지난1주간취업여부
        emp_category=14, #취업구분
        hrs_total = 17, #총취업시간
        ind_code =38,#산업코드
        occ_code=39, #직업코드
        status_worker=40, #종사상지위
        wage_monthly=42, #월평균임금
        region_work=46, #사업체소재지
        weight=47, #가중치
        econ_action_type=48 #경제활동구분
      ) %>%
      select(region, rel_head, sex, age, edu_level, marry, emp_w1, emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type ) 
    
  } else if (item$year == 2009) {
    raw <- raw %>%
      rename(
        region = 1,       
        rel_head = 2,   
        sex = 3,
        age = 4,
        edu_level =5,
        marry=8,
        emp_w1=10,
        emp_category=14,
        hrs_total=17,
        ind_code=35,
        occ_code=36,
        status_worker=37,
        wage_monthly=39,
        region_work=43,
        weight=45,
        econ_action_type=46
        
      ) %>%
      select(region, rel_head, sex, age, edu_level, marry, emp_w1, emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type)
    
  } else if (item$year == 2010) {
    raw <- raw %>%
      rename(
        region = 2,       
        rel_head = 3,   
        sex = 4,
        age = 5,
        edu_level = 6,
        marry=9,
        emp_w1=11,
        emp_category=15,
        hrs_total=18,
        ind_code=36,
        occ_code=37,
        status_worker=38,
        wage_monthly=40, 
        region_work=47,
        weight=49,
        econ_action_type=50
      ) %>%
      select(region, rel_head, sex, age, edu_level, marry, emp_w1, emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type)  
    
  } else if (item$year == 2011) {
    raw <- raw %>%
      rename(
        region = 2,       
        rel_head = 3,   
        sex = 4,
        age = 5,
        edu_level = 6,
        marry=10,
        emp_w1=12,
        emp_category = 16,    
        hrs_total = 19,      
        ind_code = 32,      
        occ_code = 33,        
        status_worker = 34,   
        wage_monthly = 36,   
        region_work = 43,    
        weight = 45,         
        econ_action_type = 46 
      ) %>%
      select(region, rel_head, sex, age, edu_level, marry, emp_w1, emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type)
    
  } else if (item$year == 2012) {
    raw <- raw %>%
      rename(
        region = 2,           
        rel_head = 3,        
        sex = 4,            
        age = 5,             
        edu_level = 6,      
        marry = 10,           
        emp_w1 = 12,          
        emp_category = 16,    
        hrs_total = 19,      
        ind_code = 32,        
        occ_code = 33,        
        status_worker = 34,  
        wage_monthly = 36,    
        region_work = 43,     
        weight = 45,         
        econ_action_type = 46 
      ) %>%
      select(region, rel_head, sex, age, edu_level, marry, emp_w1, emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type)  
    
  } else if (item$year == 2013) {
    raw <- raw %>%
      rename(
        household = 2, #신설: 가구단위일련번호
        rel_head = 3,         # 가구주관계코드
        sex = 4,              # 성별코드
        age = 5,              # 만연령
        edu_level = 6,        # 학력코드
        marry = 10,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간 일여부
        emp_category = 16,    # 주업부업총계시간구분코드 (취업구분)
        hrs_total = 19,       # 조사대상주간_근무총계시간수
        ind_code = 24,        # 2자리_10차산업중분류코드
        occ_code = 27,        # 7차직업분류코드
        status_worker = 30,   # 현직장종사상지위코드
        wage_monthly = 36,    # 최근3개월_월평균급여
        region_work = 23,     # 사업체직장소재지_행정구역코드 
        region = 54,          # 2자리_행정구역시도코드 
        weight = 55,          # 시도가중값 
        econ_action_type = 56 # 경제활동구분코드
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type)
    
  } else if (item$year == 2014) {
    raw <- raw %>%
      rename(
        household = 2,        # 가구단위일련번호
        rel_head = 3,         # 가구주관계코드
        sex = 4,              # 성별코드
        age = 5,              # 만연령
        edu_level = 6,        # 학력코드
        marry = 10,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간 일여부
        emp_category = 16,    # 주업부업총계시간구분코드
        hrs_total = 19,       # 조사대상주간_근무총계시간수
        ind_code = 24,        # 2자리_10차산업중분류코드
        occ_code = 27,        # 7차직업분류코드
        status_worker = 30,   # 현직장종사상지위코드
        wage_monthly = 36,    # 최근3개월_월평균급여
        region_work = 23,     # 사업체직장소재지_행정구역코드 
        region = 49,          # 2자리_행정구역시도코드 
        weight = 50,          # 시도가중값 
        econ_action_type = 51, # 경제활동구분코드 
        youngest_child_age=54, #막내자녀연령
        num_children=55 #18세미만자녀수
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type, youngest_child_age, num_children)  
    
  } else if (item$year == 2015) {
    raw <- raw %>%
      rename(
        household = 2,        # 가구단위일련번호
        rel_head = 3,         # 가구주관계코드
        sex = 5,              # 성별코드 
        age = 6,              # 만연령 
        edu_level = 7,        # 학력코드 
        marry = 11,           # 혼인상태코드 
        emp_w1 = 13,          # 조사대상주간_수입목적종사여부 
        emp_category = 19,    # 주업부업총계시간구분코드 
        hrs_total = 22,       # 조사대상주간_근무총계시간수
        region_work = 26,     # 사업체직장소재지_행정구역코드
        ind_code = 27,        # [수정] 2자리_10차산업중분류코드 (기존 25 -> 27)
        occ_code = 30,        # [수정] 7차직업분류코드 (기존 28 -> 30)
        status_worker = 33,   # [수정] 현직장종사상지위코드 (기존 31 -> 33)
        wage_monthly = 39,    # [수정] 최근3개월_월평균급여 (기존 37 -> 39)
        region = 46,          # 2자리_행정구역시도코드 
        weight = 47,          # 시도가중값 
        econ_action_type = 48,# 경제활동구분코드
        youngest_child_age = 51, # 막내자녀연령
        num_children = 52        # 18세미만자녀수 
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type, youngest_child_age, num_children)
    
  } else if (item$year == 2016) {
    raw <- raw %>%
      rename(
        household = 2,        # 가구단위일련번호
        rel_head = 3,         # 가구주관계코드
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 13,          # 조사대상주간_수입목적종사여부
        emp_category = 19,    # 주업부업총계시간구분코드
        hrs_total = 22,       # 조사대상주간_근무총계시간수
        ind_code = 27,        # 2자리_10차산업중분류코드 
        occ_code = 30,        # 7차직업분류코드 
        status_worker = 33,   # 현직장종사상지위코드 
        wage_monthly = 39,    # 최근3개월_월평균급여 
        region_work = 26,     # 사업체직장소재지_행정구역코드
        region = 48,          # 2자리_행정구역시도코드 
        weight = 49,          # 시도가중값
        econ_action_type = 50,# 경제활동구분코드 
        youngest_child_age = 53, # 막내자녀연령
        num_children = 54        # 18세미만자녀수 
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, region_work, weight, econ_action_type, youngest_child_age, num_children) 
    
  } else if (item$year == 2017) {
    raw <- raw %>%
      rename(
        household = 2,        # 가구단위일련번호
        rel_head = 3,         # 가구주관계코드
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 13,          # 조사대상주간_수입목적종사여부
        emp_category = 19,    # 주업부업총계시간구분코드
        hrs_total = 22,       # 조사대상주간_근무총계시간수
        region_work = 26,     # 사업체직장소재지_행정구역코드
        ind_code = 29,        # [변경] 2자리_11차산업중분류코드 (27->29)
        occ_code = 32,        # [변경] 2자리_8차직업중분류코드 (30->32)
        status_worker = 33,   # 현직장종사상지위코드
        wage_monthly = 39,    # 최근3개월_월평균급여
        region = 48,          # 2자리_행정구역시도코드 
        weight = 49,          # 시도가중값
        econ_action_type = 50,# 경제활동구분코드
        youngest_child_age = 53, # 막내자녀연령
        num_children = 54        # 18세미만자녀수
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, 
             region_work, weight, econ_action_type, youngest_child_age, num_children)    
    
  } else if (item$year == 2018) {
    raw <- raw %>%
      rename(
        household = 1,        # 가구단위일련번호 (2->1)
        rel_head = 4,         # 가구주관계코드 (3->4)
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간_수입목적종사여부
        emp_category = 20,    # 주업부업총계시간구분코드 (19->20)
        hrs_total = 19,       # 조사대상주간_근무총계시간수 (22->19)
        ind_code = 22,        # 2자리_11차산업중분류코드 (29->22)
        occ_code = 23,        # 2자리_8차직업중분류코드 (32->23)
        status_worker = 25,   # 현직장종사상지위코드 (33->25)
        wage_monthly = 28,    # 최근3개월_월평균급여 (39->28)
        region_work = 21,     # 사업체직장소재지_행정구역코드 (26->21)
        region = 49,          # 2자리_행정구역시도코드 (48->49)
        weight = 50,          # 시도가중값 (49->50)
        econ_action_type = 51,# 경제활동구분코드 (50->51)
        youngest_child_age = 54, # 막내자녀연령 (53->54)
        num_children = 55        # 18세미만자녀수 (54->55)
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, 
             region_work, weight, econ_action_type, youngest_child_age, num_children)  
    
  } else if (item$year == 2019) {
    raw <- raw %>%
      rename(
        household = 1,        # 가구단위일련번호
        rel_head = 4,         # 가구주관계코드
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간_수입목적종사여부
        emp_category = 20,    # 주업부업총계시간구분코드
        hrs_total = 19,       # 조사대상주간_근무총계시간수
        ind_code = 22,        # 2자리_11차산업중분류코드
        occ_code = 23,        # 2자리_8차직업중분류코드
        status_worker = 25,   # 현직장종사상지위코드
        wage_monthly = 28,    # 최근3개월_월평균급여
        region_work = 21,     # 사업체직장소재지_행정구역코드
        region = 49,          # 2자리_행정구역시도코드 
        weight = 50,          # 시도가중값
        econ_action_type = 51,# 경제활동구분코드
        youngest_child_age = 55, # 막내자녀연령 (54->55)
        num_children = 56        # 18세미만자녀수 (55->56)
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, 
             region_work, weight, econ_action_type, youngest_child_age, num_children) 
    
  } else if (item$year == 2020) {
    raw <- raw %>%
      rename(
        household = 1,        # 가구단위일련번호
        rel_head = 4,         # 가구주관계코드
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간_수입목적종사여부
        emp_category = 20,    # 주업부업총계시간구분코드
        hrs_total = 19,       # 조사대상주간_근무총계시간수
        ind_code = 22,        # 2자리_11차산업중분류코드
        occ_code = 23,        # 2자리_8차직업중분류코드
        status_worker = 25,   # 현직장종사상지위코드
        wage_monthly = 28,    # 최근3개월_월평균급여
        region_work = 21,     # 사업체직장소재지_행정구역코드
        region = 49,          # 2자리_행정구역시도코드 
        weight = 50,          # 시도가중값
        econ_action_type = 51,# 경제활동구분코드
        youngest_child_age = 55, # 막내자녀연령
        num_children = 56        # 18세미만자녀수
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, 
             region_work, weight, econ_action_type, youngest_child_age, num_children)  
    
  } else if (item$year == 2021) {
    raw <- raw %>%
      rename(
        household = 1,        # 가구단위일련번호
        rel_head = 4,         # 가구주관계코드
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간_수입목적종사여부
        emp_category = 20,    # 주업부업총계시간구분코드
        hrs_total = 19,       # 조사대상주간_근무총계시간수
        region_work = 21,     # 사업체직장소재지_행정구역코드
        ind_code = 23,        # [변경] 2자리_11차산업중분류코드 (22->23)
        occ_code = 24,        # [변경] 2자리_8차직업중분류코드 (23->24)
        status_worker = 26,   # [변경] 현직장종사상지위코드 (25->26)
        wage_monthly = 29,    # [변경] 최근3개월_월평균급여 (28->29)
        region = 50,          # [변경] 2자리_행정구역시도코드 (49->50)
        weight = 51,          # [변경] 시도가중값 (50->51)
        econ_action_type = 52,# [변경] 경제활동구분코드 (51->52)
        youngest_child_age = 56, # [변경] 막내자녀연령 (55->56)
        num_children = 57        # [변경] 18세미만자녀수 (56->57)
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, 
             region_work, weight, econ_action_type, youngest_child_age, num_children)  
    
  } else if (item$year == 2022) {
    
    raw <- raw %>%
      rename(
        household = 1,        # 가구단위일련번호
        rel_head = 4,         # 가구주관계코드
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간_수입목적종사여부
        emp_category = 20,    # 주업부업총계시간구분코드
        hrs_total = 19,       # 조사대상주간_근무총계시간수
        region_work = 21,     # 사업체직장소재지_행정구역코드
        ind_code = 23,        # 2자리_11차산업중분류코드
        occ_code = 24,        # 2자리_8차직업중분류코드
        status_worker = 26,   # 현직장종사상지위코드
        wage_monthly = 29,    # 최근3개월_월평균급여
        region = 50,          # 2자리_행정구역시도코드 
        weight = 51,          # 시도가중값
        econ_action_type = 52,# 경제활동구분코드
        youngest_child_age = 56, # 막내자녀연령
        num_children = 57        # 18세미만자녀수
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, 
             region_work, weight, econ_action_type, youngest_child_age, num_children) 
    
  } else if (item$year == 2023) {
    raw <- raw %>%
      rename(
        household = 1,        # 가구단위일련번호
        rel_head = 4,         # 가구주관계코드
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간_수입목적종사여부
        emp_category = 20,    # 주업부업총계시간구분코드
        hrs_total = 19,       # 조사대상주간_근무총계시간수
        region_work = 21,     # 사업체직장소재지_행정구역코드
        ind_code = 23,        # 2자리_11차산업중분류코드
        occ_code = 24,        # 2자리_8차직업중분류코드
        status_worker = 26,   # 현직장종사상지위코드
        wage_monthly = 29,    # 최근3개월_월평균급여
        region = 50,          # 2자리_행정구역시도코드 
        weight = 51,          # 시도가중값
        econ_action_type = 52,# 경제활동구분코드
        youngest_child_age = 56, # 막내자녀연령
        num_children = 57        # 18세미만자녀수
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, 
             region_work, weight, econ_action_type, youngest_child_age, num_children)  
    
    
  } else if (item$year == 2024) {
    raw <- raw %>%
      rename(
        household = 1,        # 가구단위일련번호
        rel_head = 4,         # 가구주관계코드
        sex = 5,              # 성별코드
        age = 6,              # 만연령
        edu_level = 7,        # 학력코드
        marry = 11,           # 혼인상태코드
        emp_w1 = 12,          # 조사대상주간_수입목적종사여부
        emp_category = 20,    # 주업부업총계시간구분코드
        hrs_total = 19,       # 조사대상주간_근무총계시간수
        region_work = 21,     # 사업체직장소재지_행정구역코드
        ind_code = 23,        # 2자리_11차산업중분류코드
        occ_code = 24,        # 2자리_8차직업중분류코드
        status_worker = 26,   # 현직장종사상지위코드
        wage_monthly = 29,    # 최근3개월_월평균급여
        region = 50,          # 2자리_행정구역시도코드 
        weight = 51,          # 시도가중값
        econ_action_type = 52,# 경제활동구분코드
        youngest_child_age = 56, # 막내자녀연령
        num_children = 57        # 18세미만자녀수
      ) %>%
      select(region, household, rel_head, sex, age, edu_level, marry, emp_w1, 
             emp_category, hrs_total, ind_code, occ_code, status_worker, wage_monthly, 
             region_work, weight, econ_action_type, youngest_child_age, num_children)    
    
  } 
  
  raw$year <- item$year #year variable
  
  # save to list and clean
  all_data[[paste0("yr_", item$year)]] <- raw
  rm(raw, mdis) 
}


### merge data frames into one

all_data <- lapply(all_data, function(df) {
  df %>%
    mutate(
      weight = as.numeric(weight),
      age = as.numeric(age),
      hrs_total = as.numeric(hrs_total),
      wage_monthly = as.numeric(wage_monthly),
      status_worker =as.numeric(status_worker)
    )
})

df <- bind_rows(all_data)

dim(df)
table(df$year) 

### clean region

df <- df %>%
  mutate(
    region_do = case_when(
      str_detect(region, "^서울") ~ "서울",
      str_detect(region, "^부산") ~ "부산",
      str_detect(region, "^대구") ~ "대구",
      str_detect(region, "^인천") ~ "인천",
      str_detect(region, "^대전") ~ "대전",
      str_detect(region, "^울산") ~ "울산",
      str_detect(region, "^세종") ~ "세종",
      str_detect(region, "^제주") ~ "제주",
      str_detect(region, "^강원") ~ "강원",
      str_detect(region, "^경기") ~ "경기",
      str_detect(region, "^충북|^충청북") ~ "충북",
      str_detect(region, "^충남|^충청남") ~ "충남",
      str_detect(region, "^전북|^전라북") ~ "전북",
      str_detect(region, "^전남|^전라남") ~ "전남",
      str_detect(region, "^경북|^경상북") ~ "경북",
      str_detect(region, "^경남|^경상남") ~ "경남",
      
      region == "광주" | str_detect(region, "광주광역시") ~ "광주", 
      str_detect(region, "광주시") ~ "경기",
      
      region == "고성군" ~ "경남", 
      
      str_detect(region, "수원|성남|의정부|안양|부천|광명|평택|동두천|안산|고양|과천|구리|남양주|오산|시흥|군포|의왕|하남|용인|파주|이천|안성|김포|화성|양주|포천|여주|연천|가평|양평") ~ "경기",
      
      str_detect(region, "춘천|원주|강릉|동해|태백|속초|삼척|홍천|횡성|영월|평창|정선|철원|화천|양구|인제|양양") ~ "강원",
      
      str_detect(region, "청주|충주|제천|청원|보은|옥천|영동|증평|진천|괴산|음성|단양") ~ "충북",
      
      str_detect(region, "천안|공주|보령|아산|서산|논산|계룡|당진|금산|연기|부여|서천|청양|홍성|예산|태안") ~ "충남",
      
      str_detect(region, "전주|군산|익산|정읍|남원|김제|완주|진안|무주|장수|임실|순창|고창|부안") ~ "전북",
      
      str_detect(region, "목포|여수|순천|나주|광양|담양|곡성|구례|고흥|보성|화순|장흥|강진|해남|영암|무안|함평|영광|장성|완도|진도|신안") ~ "전남",
      
      str_detect(region, "포항|경주|김천|안동|구미|영주|영천|상주|문경|경산|군위|의성|청송|영양|영덕|청도|고령|성주|칠곡|예천|봉화|울진|울릉") ~ "경북",
      
      str_detect(region, "창원|마산|진해|진주|통영|사천|김해|밀양|거제|양산|의령|함안|창녕|남해|하동|산청|함양|거창|합천") ~ "경남",
      
      str_detect(region, "서귀포|제주") ~ "제주",
      
      TRUE ~ as.character(region)
    )
  )


#export cleaned data
write_csv(df, file.path(out_dir, "valuesRaw.csv"))



