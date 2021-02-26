## Place in the R subfolder


## r pop-5yr
## https://www.populationpyramid.net/united-states-of-america/
## 5-year intervals to 100+
usURL <- "https://www.populationpyramid.net/api/pp/840/2020/?csv=true"
usPop5s <- read.csv(usURL)
italyURL <- "https://www.populationpyramid.net/api/pp/380/2020/?csv=true"
italyPop5s <- read.csv(italyURL)
germanURL <- "https://www.populationpyramid.net/api/pp/276/2020/?csv=true"
germanPop5s <- read.csv(germanURL)
kenyaURL <- "https://www.populationpyramid.net/api/pp/404/2020/?csv=true"
kenyaPop5s <- read.csv(kenyaURL)

# library(httr)
# GET(italyURL, authenticate(":", ":", type = "ntlm"), write_disk('italyPop5s.csv'))
# GET(germanRL, authenticate(":", ":", type = "ntlm"), write_disk('germanPop5s.csv'))
# GET(germanURL, authenticate(":", ":", type = "ntlm"), write_disk('germanPop5s.csv'))
# GET(kenyaURL, authenticate(":", ":", type = "ntlm"), write_disk('kenyaPop5s.csv'))

## https://data.cdc.gov/resource/3apk-4u4f/

## r statsByYr
library(openxlsx)
## US deaths, Covid-19 and total, by sex and age (yr) to 85+, at January 30 2021
deadBy1yr <- read.csv('https://data.cdc.gov/api/views/3apk-4u4f/rows.csv?accessType=DOWNLOAD')
# deathsByAge => deadBy1yr  ageUSpop => USpop1yr  ageUSdeaths => USdeadpop1yr
wkStartEnd <- format(as.Date(as.character(deadBy1yr[1,2:3]),"%m/%d/%Y"),
                     format="%b %d %Y")
url <- paste0('https://www2.census.gov/programs-surveys/popest/tables/',
              '2020/2020-demographic-analysis-estimates/table-3.xlsx')
USpop1yr <- read.xlsx(url, rows=c(2,5:90))[,c(1,4,7)]
USpop1yr[['age']] <- c(0:84,Inf)
## r matchInfo
USdeadpop1yr <- with(deadBy1yr,
                    cbind(age=c(0:84,Inf),
                          covdeaths=xtabs(COVID.19.Deaths~Age.Years),
                          alldeaths=xtabs(Total.deaths~Age.Years),
                          pop=USpop1yr[,'Total.resident.population']*1000))
if(!all(USpop1yr[-86,'Age.on.April.1,.2020']==USdeadpop1yr[-86,"age"]))
  print("Mismatch of age details")
USdeadpop1yr <- within(data.frame(USdeadpop1yr), {relcovdead<-covdeaths/pop;
reldead<-alldeaths/pop; age<-as.numeric(age)})

## Case Surveillance Public Use Data
## Deaths by cause (broad classification) and age, US
## https://www.cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm
## Provisional COVID-19 Death Counts by Sex, Age, and State
## Also shows cause --- Table 1 shows US data)
## deathsByCause => UScausesByCats
url <- "https://data.cdc.gov/api/odata/v4/9bhg-hcku"
library(RSocrata)
UScausesByCats <- subset(read.socrata(url),
                        state=='United States'&sex=='All Sexes')
colnam <- UScausesByCats[['age_group_new']][c(1,2,4,5,6,8,10:11,13:15,16)]
cnam <- c('<1', gsub(' years','', colnam[-c(1:2,12)]),'85+')
totalsByCause <- subset(UScausesByCats, age_group_new=="All Ages")
UScausesByCats <- within(subset(UScausesByCats, age_group_new%in%colnam[-1]),
                        gp<-factor(age_group_new, levels=age_group_new, labels=cnam))
if(!all(sapply(UScausesByCats[,7:12],sum)==totalsByCause[7:12])){
  print(rbind(sapply(UScausesByCats[,7:12],sum),totalsByCause[7:12]))
  stop("Inconsistency in data")
}

newlims <- c(0,4,14,24,34,44,54,64,74,84,Inf)
USpop1yr <- within(USpop1yr, cumpop <- cumsum(Total.resident.population))
nr <- match(newlims, USpop1yr$age)
UScausesByCats <- within(UScausesByCats, {age_group_new<- factor(age_group_new,
                      levels=age_group_new,labels=cnam);
                      pop <- diff(c(0,USpop1yr[['cumpop']][nr]));
                      pneumonia_alone_r <- (pneumonia_deaths-pneumonia_and_covid_19_deaths)/
                        pop; covid_19_r <- covid_19_deaths/pop;
                        influenza_all_r <- influenza_deaths/pop})

## Number of covid-19 deaths, by country, from "Our World in Data":
url <- "https://covid.ourworldindata.org/data/owid-covid-data.xlsx"
byCountry <- subset(read.xlsx(url), date=="2021-01-30")
codes <- c('USA','ITA','DEU','KEN')
nr <- match(codes,byCountry[["iso_code"]])
cf4countries <- byCountry[nr,c('date','population','total_deaths')]
rownames(cf4countries) <- codes

save(usPop5s, italyPop5s, germanPop5s, kenyaPop5s,USdeadpop1yr,wkStartEnd, UScausesByCats,
     cf4countries, file="UScovidLatest.RData")

## US cases and deaths by state and time
## url <-"https://data.cdc.gov/api/odata/v4/9mfq-cb36"
## cANDd <- read.socrata(url)
## tot_death<-sum(subset(cANDd,submission_date<as.Date("2021-01-04"))[,'new_death'])
## 352464
