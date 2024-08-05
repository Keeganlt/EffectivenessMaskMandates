###################################################### 
# Assessing the Impact of Mask Mandates on SARS-CoV-2 
## Transmission: A Case Study of Utah
#
# Alicia Horn, Holly Shoemaker, Lindsay Keegan
# Updated Aug 5, 2024
###################################################### 


# Packages ----------------------------------------------------------------
library(dplyr)
library(lubridate)
library(tidyverse)
library(readr)
library(maps)
library(ggplot2)
library(segmented)
library(cowplot)
theme_set(theme_cowplot(font_size=18))
library(scales)
#library(rgdal)
library(sf)
library(ggplot2)
library(ggnewscale)


# Variants ----------------------------------------------------------------

## Load in data ------------------------------------------------------------

udoh.variants <- read.csv(file = "Testing_Sequencing Results by Week_2024-07-24.csv")

udoh.variants <- udoh.variants %>%
  mutate(Week.Sample.was.Collected = lubridate::mdy(Week.Sample.was.Collected))

udoh.variants.filtered <- udoh.variants %>%
  filter(Week.Sample.was.Collected >= as.Date("2020-10-25")) %>%
  filter(Week.Sample.was.Collected <= as.Date("2021-05-08"))

# Calculate the total number of cases per week
udoh.variants.filtered.total <- udoh.variants.filtered %>%
  group_by(Week.Sample.was.Collected) %>%
  summarize(Total_Identified = sum(Number.Identified.in.Utah, na.rm = TRUE), .groups = 'drop')

# Merge the totals back into the original dataframe
udoh.variants.filtered <- udoh.variants.filtered %>%
  left_join(udoh.variants.filtered.total, by = "Week.Sample.was.Collected")

# Calculate the percentage for each lineage within each week
udoh.variants.filtered <- udoh.variants.filtered %>%
  mutate(Percentage = (Number.Identified.in.Utah / Total_Identified) * 100)

percents<-udoh.variants.filtered %>%
  group_by(Week.Sample.was.Collected) %>%
  summarize(Total_percent = sum(Percentage, na.rm = TRUE), .groups = 'drop')

udoh.variants.filtered <- udoh.variants.filtered %>%
  mutate(Lineage = factor(Lineage, levels = unique(Lineage)))


ggplot(udoh.variants.filtered, aes(x = Week.Sample.was.Collected, y = Percentage, fill = Lineage)) + 
  geom_bar(position="stack", stat="identity", width = 7) +
  xlab("Week Sample Collected") + 
  ylab("% Identified ") + 
  theme_bw() + 
  scale_fill_discrete(
    labels = c("B.1.1.7" = "Alpha", "B.1.351" = "Beta", "B.1.427" = "Epsilon1",
               "B.1.429" = "Epsilon2", "B.1.617.2" = "Delta",
               "Other Lineage" = "Other\nLineage", "P.1" = "Gamma", "B.1.1.529" = "Omicron")) +
  theme(legend.position = "bottom")+
  geom_vline(xintercept = as.Date("2020-06-28", "%Y-%m-%d")) + 
  annotate("text", x= (as.Date("2020-07-10", "%Y-%m-%d")+1), y = 40, label = "SLSC Mandate", angle=270, hjust = 0, size = 3.5) +
  geom_vline(xintercept = as.Date("2020-11-09", "%Y-%m-%d")) +
  annotate("text", x= (as.Date("2020-11-20", "%Y-%m-%d")+1), y = 49, label = "Statewide Mandate", angle=270, hjust = 0, size = 3.5) +
  geom_vline(xintercept = as.Date("2021-04-10", "%Y-%m-%d")) +
  annotate("text", x= (as.Date("2021-04-20", "%Y-%m-%d")+1), y = 62, label = "Statewide Mandate Lifted", angle=270, hjust = 0, size = 3.5) + 
  xlim(as_date("2020-06-01"), as_date("2021-05-09"))



# Incidence Plots  --------------------------------------------------------

## Load in data ------------------------------------------------------------

# load in data
udohh.incid <- read.csv("Overview_COVID-19 Cases by the Date a Positive Test was Reported to Public Health by LHD_2021-09-03.csv")
#read.csv("C:/Users/Alici/OneDrive/Documents/Utah_COVID19_data (1)/Overview_COVID-19 Cases by the Date a Positive Test was Reported to Public Health by LHD_2021-09-03.csv")

# converting date
udohh.incid$Date <- as.Date(udohh.incid$Date, "%Y-%m-%d")

## visualize and summarize the data
summary(udohh.incid)
View(udohh.incid)


## Figure 1: Incidence  ----------------------------------------------------

udohhplot <- ggplot(udohh.incid) +
  geom_line(aes(x = Date, y = Count, color = jurisdiction)) +
  theme_cowplot(20) +
  geom_vline(xintercept = as.Date("2020-06-10", "%Y-%m-%d")) + 
  annotate("text", x= (as.Date("2020-07-01", "%Y-%m-%d")-3), y = 1750, label = "Salt Lake and Summit County Mandate", angle=270, hjust = 0, size = 5) +
  geom_vline(xintercept = as.Date("2020-11-09", "%Y-%m-%d")) +
  annotate("text", x= (as.Date("2020-11-20", "%Y-%m-%d")+1), y = 1750, label = "Statewide Mandate", angle=270, hjust = 0, size = 5) +
  geom_vline(xintercept = as.Date("2021-04-10", "%Y-%m-%d")) +
  annotate("text", x= (as.Date("2021-04-20", "%Y-%m-%d")+1), y = 1750, label = "Statewide Mandate Lifted", angle=270, hjust = 0, size = 5) + 
  labs(y = "Incident Cases", x= "Date")  +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + 
  theme(axis.text.x=element_text(angle=60, hjust=1), 
        legend.position = "bottom") + 
  guides(color=guide_legend(title="Local Health\nDistrict")) 

 g2 <- udohhplot + theme(legend.position = "none")


legend_2 <- cowplot::get_plot_component(udohhplot, "guide-box", return_all = TRUE)[[3]]


mandatedcount <- udohh.incid %>%
  filter(jurisdiction %in% c("Salt Lake County", "Summit County"))

mandatedcountplot <- ggplot(mandatedcount) +
  geom_line(aes(x = Date, y = Count, color = jurisdiction))+ 
  theme_cowplot(20) +
  geom_vline(xintercept = as.Date("2020-06-10", "%Y-%m-%d")) + 
  annotate("text", x= (as.Date("2020-07-01", "%Y-%m-%d")-3), y = 1750, label = "Salt Lake and Summit County Mandate", angle=270, hjust = 0, size = 5) +
  geom_vline(xintercept = as.Date("2020-11-09", "%Y-%m-%d")) +
  annotate("text", x= (as.Date("2020-11-20", "%Y-%m-%d")+1), y = 1750, label = "Statewide Mandate", angle=270, hjust = 0, size = 5) +
  geom_vline(xintercept = as.Date("2021-04-10", "%Y-%m-%d")) +
  annotate("text", x= (as.Date("2021-04-20", "%Y-%m-%d")+1), y = 1750, label = "Statewide Mandate Lifted", angle=270, hjust = 0, size = 5) + 
  scale_color_manual(values = c("#7CAE00", "#00B4F0")) +
  labs(y = "Incident Cases", x= "Date")  +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) +  
  theme(legend.position = "none")

g1 <- mandatedcountplot

#mortality rate
#load in data
# deaths <- read.csv("C:/Users/Alici/OneDrive/Documents/Utah_COVID19_data (1)/Overview_Seven-Day Rolling Average Mortality by Date of Death_2021-09-03.csv")
deaths <- read.csv("~/Downloads/Utah_COVID19_data/Overview_Seven-Day Rolling Average Mortality by Date of Death_2024-07-24.csv")
summary(deaths)
#converting date
deaths$Date <- lubridate::ymd(deaths$Date)
View(deaths)

deaths %>% 
  filter(Date <= as.Date("2021-06-28", "%Y-%m-%d")
         & Date >= as.Date("2020-04-28", "%Y-%m-%d")) -> smalldeaths
#udohh <- group_by(as.Date())
deathsplot <- ggplot(smalldeaths) +
  geom_line(aes(x = Date, y = Mortality.Count), color = "navy")+
  geom_vline(xintercept = as.Date("2020-06-28", "%Y-%m-%d"), size = 0.9) + 
  annotate("text", x= as.Date("2020-07-09", "%Y-%m-%d"), y = 25, label = "SLSC Mandate", angle=270) +
  geom_vline(xintercept = as.Date("2020-11-09", "%Y-%m-%d"), size = 0.9) +
  annotate("text", x= as.Date("2020-11-20", "%Y-%m-%d"), y = 1, label = "Statewide Mandate", angle=270) +
  geom_vline(xintercept = as.Date("2021-04-10", "%Y-%m-%d"), size = 0.9) +
  annotate("text", x= as.Date("2021-04-20", "%Y-%m-%d"), y = 20, label = "Statewide Mandate Lifted", angle=270) +
  xlab("Date") +
  ylab("Daily Mortality Count")

deathsplot


space<- ggplot() + theme_void() + 
  theme(panel.background = element_rect(fill = "transparent", color = NA))


Fig.1a <- plot_grid(g1, g2,labels = c("A","B"), nrow = 1)
Fig.1b <- plot_grid(space, legend_2, nrow = 1, rel_widths = c(0.01,1))


Fig.1ab <- plot_grid(Fig.1a, Fig.1b, nrow = 2, rel_heights = c(1,0.2))


## FIGURE 1: Data 
Fig.1 <- plot_grid(g1, g2, legend_2, labels = c("A","B",""), nrow = 1, rel_widths = c(1,1,0.3))

Fig.1

Fig1.all <- plot_grid(Fig.1, deathsplot, labels = c("", "C"), nrow = 2)

# Efm Calculations --------------------------------------------------------

## Load in data ------------------------------------------------------------

# load in data
udohh <- read.csv("Overview_COVID-19 Daily Case Rate by LHD_2021-09-03.csv")
# ("C:/Users/Alici/OneDrive/Documents/Utah_COVID19_data (1)/Overview_COVID-19 Daily Case Rate by LHD_2021-09-03.csv")
summary(udohh)

#converting date
udohh$Date <- as.Date(udohh$Date, "%Y-%m-%d")

## Colors for maps
colour_breaks <- c(-50, 0, 50, 105)
colours <- c("#67a9cf",
                      "#f7f7f7",
                      "#ef8a62",
                      "#a23a11")

# Efm Salt lake/summit county mandate -------------------------------------

udohh %>% 
  filter(Date <= as.Date("2020-08-28", "%Y-%m-%d")
         & Date >= as.Date("2020-04-28", "%Y-%m-%d")) -> smalll.slsc 
### Key Parameters ----------------------------------------------------------

SLCMaskMandate <- "2020-06-28" #Date of the SLC mask mandate
NumberDays <- 28 # Number of days pre/post mandate that we are comparing
SLCPreMaskMandate <- "2020-06-27"
SLCPostMaskMandate <- "2020-06-29"
#generation time
mean.gt <- 6.5 # mean
stdev.gt <- 0.82 # stdev

# Filter data by county
#Making datasets for each group
JurisFun <- function(CountySelected){
  smalll.slsc %>% 
    filter(jurisdiction == CountySelected) -> CountyOutput 
  return(CountyOutput)
} 


# Calculate Start and End Case Rate per 100,000 pre mandate 
PreMdtCaseRtFun <- function(county, StartDate, NumDays){
  CaseRtStart <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")- NumDays) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRtEnd <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRt<-c(CaseRtStart, CaseRtEnd)
  return(CaseRt)
}




# Calculate Start and End Case Rate per 100,000 post mandate 

PostMdtCaseRtFun <- function(county, StartDate, NumDays){
  CaseRtStart <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")+1) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRtEnd <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d") + 1 + NumDays) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRt<-c(CaseRtStart, CaseRtEnd)
  return(CaseRt)
}




# Growth rate after regulations 
MaskGrowthFun <- function(mu, sigma, EndCaseRt, StartCaseRt, NumDays){
  rgrow <- (log(EndCaseRt/StartCaseRt)/NumDays) 
  c <- sigma/mu
  c2min <- c^-2
  Rgrow <- (((c^2)*rgrow*mu)+1)^c2min
  return(Rgrow)
}




# Final Calculation
EFmMaskFun <- function(Rmask, Rno){
  EFmMask <- 1-(Rmask/Rno)
  return(EFmMask)
}

### Salt Lake County (SLCC) --------------------------------------------------------

slcc <- JurisFun(CountySelected= "Salt Lake County")
PreMdtCaseRtOutSlcc<-PreMdtCaseRtFun(slcc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutSlcc<-PostMdtCaseRtFun(slcc, SLCMaskMandate, NumberDays)
RnoSLC<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutSlcc[2], StartCaseRt= PreMdtCaseRtOutSlcc[1], NumDays = NumberDays)
RmaskSLC<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutSlcc[2], StartCaseRt= PostMdtCaseRtOutSlcc[1], NumDays = NumberDays)
EFmMaskSLC <- EFmMaskFun(Rmask = RmaskSLC,Rno = RnoSLC)
EFmMaskSLC*100



EFmMaskSLC
EFmPercentNegSLC <- cbind(EFmMaskSLC*-100, c("Salt Lake County"))
colnames(EFmPercentNegSLC) <- c("EFm", "LHD")

### Summit Lake County (SumC)--------------------------------------------------------

sumc <- JurisFun(CountySelected = "Summit County")
PreMdtCaseRtOutsumc<-PreMdtCaseRtFun(sumc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutsumc<-PostMdtCaseRtFun(sumc, SLCMaskMandate, NumberDays)
Rnosumc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutsumc[2], StartCaseRt= PreMdtCaseRtOutsumc[1], NumDays = NumberDays)
Rmasksumc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutsumc[2], StartCaseRt= PostMdtCaseRtOutsumc[1], NumDays = NumberDays)
EFmMasksumc <- EFmMaskFun(Rmask = Rmasksumc,Rno = Rnosumc)
EFmMasksumc*100

EFmMasksumc
EFmPercentNegSummit <- cbind(EFmMasksumc*-100, c("Summit County"))
colnames(EFmPercentNegSummit) <- c("EFm", "LHD")


### Bear River County (BRC) --------------------------------------------------------

brc <- JurisFun(CountySelected = "Bear River")
PreMdtCaseRtOutbrc<-PreMdtCaseRtFun(brc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutbrc<-PostMdtCaseRtFun(brc, SLCMaskMandate, NumberDays)
Rnobrc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutbrc[2], StartCaseRt= PreMdtCaseRtOutbrc[1], NumDays = NumberDays)
Rmaskbrc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutbrc[2], StartCaseRt= PostMdtCaseRtOutbrc[1], NumDays = NumberDays)
EFmMaskbrc <- EFmMaskFun(Rmask = Rmaskbrc,Rno = Rnobrc)
EFmMaskbrc*100

EFmMaskbrc
EFmPercentNegBRC <- cbind(EFmMaskbrc*-100, c("Bear River"))
colnames(EFmPercentNegBRC) <- c("EFm", "LHD")


### Utah County (UCC) --------------------------------------------------------

ucc <- JurisFun(CountySelected = "Utah County")
PreMdtCaseRtOutucc<-PreMdtCaseRtFun(ucc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutucc<-PostMdtCaseRtFun(ucc, SLCMaskMandate, NumberDays)
Rnoucc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutucc[2], StartCaseRt= PreMdtCaseRtOutucc[1], NumDays = NumberDays)
Rmaskucc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutucc[2], StartCaseRt= PostMdtCaseRtOutucc[1], NumDays = NumberDays)
EFmMaskucc <- EFmMaskFun(Rmask = Rmaskucc,Rno = Rnoucc)
EFmMaskucc*100

EFmMaskucc
EFmPercentNegUtah <- cbind(EFmMaskucc*-100, c("Utah County"))
colnames(EFmPercentNegUtah) <- c("EFm", "LHD")


### Davis County (DC) --------------------------------------------------------

dc <- JurisFun(CountySelected = "Davis County")
PreMdtCaseRtOutdc<-PreMdtCaseRtFun(dc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutdc<-PostMdtCaseRtFun(dc, SLCMaskMandate, NumberDays)
Rnodc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutdc[2], StartCaseRt= PreMdtCaseRtOutdc[1], NumDays = NumberDays)
Rmaskdc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutdc[2], StartCaseRt= PostMdtCaseRtOutdc[1], NumDays = NumberDays)
EFmMaskdc <- EFmMaskFun(Rmask = Rmaskdc,Rno = Rnodc)
EFmMaskdc*100

EFmMaskdc
EFmPercentNegDavis <- cbind(EFmMaskdc*-100, c("Davis County"))
colnames(EFmPercentNegDavis) <- c("EFm", "LHD")

### Tooele County (TC) --------------------------------------------------------

#### also problematic
tc <- JurisFun(CountySelected = "Tooele County")
PreMdtCaseRtOuttc<-PreMdtCaseRtFun(tc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOuttc<-PostMdtCaseRtFun(tc, SLCMaskMandate, NumberDays)
Rnotc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOuttc[2], StartCaseRt= PreMdtCaseRtOuttc[1], NumDays = NumberDays)
Rmasktc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOuttc[2], StartCaseRt= PostMdtCaseRtOuttc[1], NumDays = NumberDays)
EFmMasktc <- EFmMaskFun(Rmask = Rmasktc,Rno = Rnotc)
EFmMasktc*100

EFmMasktc
EFmPercentNegTooele <- cbind(EFmMasktc*-100, c("Tooele County"))
colnames(EFmPercentNegTooele) <- c("EFm", "LHD")


### Central Utah LHD (CU) --------------------------------------------------------

cu <- JurisFun(CountySelected = "Central Utah")
PreMdtCaseRtOutcu<-PreMdtCaseRtFun(cu, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutcu<-PostMdtCaseRtFun(cu, SLCMaskMandate, NumberDays)
Rnocu<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutcu[2], StartCaseRt= PreMdtCaseRtOutcu[1], NumDays = NumberDays)
Rmaskcu<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutcu[2], StartCaseRt= PostMdtCaseRtOutcu[1], NumDays = NumberDays)
EFmMaskcu <- EFmMaskFun(Rmask = Rmaskcu,Rno = Rnocu)
EFmMaskcu*100

EFmMaskcu
EFmPercentNegCentral <- cbind(EFmMaskcu*-100, c("Central Utah"))
colnames(EFmPercentNegCentral) <- c("EFm", "LHD")


### #San Juan County (SJ) --------------------------------------------------------

sj <- JurisFun(CountySelected = "San Juan")
PreMdtCaseRtOutsj<-PreMdtCaseRtFun(sj, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutsj<-PostMdtCaseRtFun(sj, SLCMaskMandate, NumberDays)
Rnosj<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutsj[2], StartCaseRt= PreMdtCaseRtOutsj[1], NumDays = NumberDays)
Rmasksj<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutsj[2], StartCaseRt= PostMdtCaseRtOutsj[1], NumDays = NumberDays)
EFmMasksj <- EFmMaskFun(Rmask = Rmasksj,Rno = Rnosj)
EFmMasksj*100

EFmMasksj
EFmPercentNegSanJuan <- cbind(EFmMasksj*-100, c("San Juan"))
colnames(EFmPercentNegSanJuan) <- c("EFm", "LHD")

### Southeast Utah LHD (SEU) --------------------------------------------------------

seu <- JurisFun(CountySelected = "Southeast Utah")
PreMdtCaseRtOutseu<-PreMdtCaseRtFun(seu, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutseu<-PostMdtCaseRtFun(seu, SLCMaskMandate, NumberDays)
Rnoseu<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutseu[2], StartCaseRt= PreMdtCaseRtOutseu[1], NumDays = NumberDays)
Rmaskseu<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutseu[2], StartCaseRt= PostMdtCaseRtOutseu[1], NumDays = NumberDays)
EFmMaskseu <- EFmMaskFun(Rmask = Rmaskseu,Rno = Rnoseu)
EFmMaskseu*100

EFmMaskseu
EFmPercentNegSoutheast <- cbind(EFmMaskseu*-100, c("Southeast Utah"))
colnames(EFmPercentNegSoutheast) <- c("EFm", "LHD")

### Southwest Utah LHD (SWU) --------------------------------------------------------

swu <- JurisFun(CountySelected = "Southwest Utah")
PreMdtCaseRtOutswu<-PreMdtCaseRtFun(swu, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutswu<-PostMdtCaseRtFun(swu, SLCMaskMandate, NumberDays)
Rnoswu<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutswu[2], StartCaseRt= PreMdtCaseRtOutswu[1], NumDays = NumberDays)
Rmaskswu<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutswu[2], StartCaseRt= PostMdtCaseRtOutswu[1], NumDays = NumberDays)
EFmMaskswu <- EFmMaskFun(Rmask = Rmaskswu,Rno = Rnoswu)
EFmMaskswu*100

EFmMaskswu
EFmPercentNegSouthwest <- cbind(EFmMaskswu*-100, c("Southwest Utah"))
colnames(EFmPercentNegSouthwest) <- c("EFm", "LHD")

### TriCounty LHD (TRC) --------------------------------------------------------

trc <- JurisFun(CountySelected = "TriCounty")
PreMdtCaseRtOuttrc<-PreMdtCaseRtFun(trc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOuttrc<-PostMdtCaseRtFun(trc, SLCMaskMandate, NumberDays)
Rnotrc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOuttrc[2], StartCaseRt= PreMdtCaseRtOuttrc[1], NumDays = NumberDays)
Rmasktrc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOuttrc[2], StartCaseRt= PostMdtCaseRtOuttrc[1], NumDays = NumberDays)
EFmMasktrc <- EFmMaskFun(Rmask = Rmasktrc,Rno = Rnotrc)
EFmMasktrc*100

EFmMasktrc
EFmPercentNegTricounty <- cbind(EFmMasktrc*-100, c("TriCounty"))
colnames(EFmPercentNegTricounty) <- c("EFm", "LHD")

### Weeber-Morgan LHD (WMC) --------------------------------------------------------

wmc <- JurisFun(CountySelected = "Weber-Morgan")
PreMdtCaseRtOutwmc<-PreMdtCaseRtFun(wmc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutwmc<-PostMdtCaseRtFun(wmc, SLCMaskMandate, NumberDays)
Rnowmc<-MaskGrowthFun(mu=mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutwmc[2], StartCaseRt= PreMdtCaseRtOutwmc[1], NumDays = NumberDays)
Rmaskwmc<-MaskGrowthFun(mu=mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutwmc[2], StartCaseRt= PostMdtCaseRtOutwmc[1], NumDays = NumberDays)
EFmMaskwmc <- EFmMaskFun(Rmask = Rmaskwmc,Rno = Rnowmc)
EFmMaskwmc*100

EFmMaskwmc
EFmPercentNegwebermorgan <- cbind(EFmMaskwmc*-100, c("Weber-Morgan"))
colnames(EFmPercentNegwebermorgan) <- c("EFm", "LHD")


### Wasatch County (WC) --------------------------------------------------------

wc <- JurisFun(CountySelected = "Wasatch County")
PreMdtCaseRtOutwc<-PreMdtCaseRtFun(wc, SLCMaskMandate, NumberDays)
PostMdtCaseRtOutwc<-PostMdtCaseRtFun(wc, SLCMaskMandate, NumberDays)
Rnowc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PreMdtCaseRtOutwc[2], StartCaseRt= PreMdtCaseRtOutwc[1], NumDays = NumberDays)
Rmaskwc<-MaskGrowthFun(mu=mean.gt, sigma = stdev.gt, EndCaseRt = PostMdtCaseRtOutwc[2], StartCaseRt= PostMdtCaseRtOutwc[1], NumDays = NumberDays)
EFmMaskwc <- EFmMaskFun(Rmask = Rmaskwc,Rno = Rnowc)
EFmMaskwc*100

EFmMaskwc
EFmPercentNegwasatch <- cbind(EFmMaskwc*-100, c("Wasatch County"))
colnames(EFmPercentNegwasatch) <- c("EFm", "LHD")


## Combining all Efm values -------------------------------------------------------

EFmAll <- as.data.frame(rbind(EFmPercentNegSLC, EFmPercentNegSummit, 
                              EFmPercentNegwasatch, EFmPercentNegwebermorgan, 
                              EFmPercentNegTricounty, EFmPercentNegSouthwest, 
                              EFmPercentNegSoutheast, EFmPercentNegSanJuan, 
                              EFmPercentNegCentral, EFmPercentNegTooele, 
                              EFmPercentNegDavis, EFmPercentNegUtah, 
                              EFmPercentNegBRC))

EFmAll 


## Map Making -------------------------------------------------------
#problem is that names don't match, edit LHD-county file

## read in map data
county <- read.csv("LHD-county.csv")
  #read.csv("C:/Users/Alici/OneDrive/Downloads/LHD-county.csv")

#roads <- rgdal::readOGR("/Utah_Roads/Roads.shp")


# Load shapefile of Utah roads (replace "path/to/utah_roads.shp" with actual file path)
utah_roads <- st_read("Utah_Roads/Roads.shp")

# Filter for Interstate 15
i15 <- utah_roads[utah_roads$CARTOCODE == 1, ]


merge(EFmAll, county, by.x = "LHD", by.y = "LHD", all.x = TRUE
      , all.y = FALSE
) -> EFmAll_county

Utah <- map_data("county","Utah")

## there will be warnings here, ignore them, there are multiple counties in most LHDs
Utah_final <- inner_join(EFmAll_county, Utah, by=c('County' = 'subregion'))


Utah_final$EFm <- as.numeric(Utah_final$EFm)

utah_base <- ggplot() + 
  coord_fixed(1.3) + 
  geom_polygon(data = Utah_final, mapping = aes(x = long, y = lat, group = County), 
               color = "black", fill = "gray") 
utah_base



ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# utah_base + 
#   geom_polygon(data = Utah_final, 
#                mapping = aes(x = long, y = lat, group = County, fill = EFm), 
#                color = "white") +
#   geom_polygon(data = Utah_final, mapping = aes(x = long, y = lat, group = County),
#                color = "black", fill = NA) +
#   theme_bw() +
#   ditch_the_axes +
#   scale_fill_gradient2(low = "#67a9cf",
#                        mid = "#f7f7f7",
#                        high = "#ef8a62",
#                        midpoint = 0.0,
#                        limits = c(-50,50)) +
#   geom_sf(data = i15, aes(color = CARTOCODE), size = 2, show.legend = "line") + 
#   scale_color_manual(values = "maroon", 
#                      labels = c("Interstate Highways"),
#                      name = "Roads") 


#MAKING MAP WITH JURISDICTION LINES
Utah_final <- inner_join(EFmAll_county, Utah, by=c('County' = 'subregion'))
Utah_final$EFm <- as.numeric(Utah_final$EFm)

write.csv(Utah_final, "Mask.Mandate.csv")
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


# Read the health districts shapefile

health_districts <- st_read("Utah_Health_Care_Districts/")
## st_read("C:/Users/Alici/Downloads/Utah_Health_Care_Districts/")

old.mask.data <- read.csv("Mask.Mandate.csv")

## ALICIA: You can either repeat this step 3 times, or merge 
old.mask.data <- old.mask.data %>% 
  dplyr::select(LHD, EFm) %>%
  group_by(LHD) %>% 
  slice_head(n = 1)


new.mask.data <-merge(health_districts, old.mask.data, by.x = c("DISTNAME"), 
                      by.y = c("LHD"), all.x = TRUE, all.y = FALSE)



# Get the Utah county boundaries from the maps package
utah_counties <- map("county", "utah", plot = FALSE, fill = TRUE)

# Convert the county boundaries to an sf object
utah_counties <- st_as_sf(utah_counties)


# Highlight Salt Lake County
slco_counties <- map("county", "utah,salt lake", plot = FALSE, fill = TRUE)

# Convert the county boundaries to an sf object
slco_counties <- st_as_sf(slco_counties)



# Get the map data for Salt Lake County
salt_lake_map <- map("county", "utah,salt lake", plot = FALSE, fill = TRUE)

# Convert to an sf object
salt_lake_sf <- st_as_sf(map2SpatialPolygons(salt_lake_map, IDs = salt_lake_map$names, 
                                             proj4string = CRS("+proj=longlat +datum=WGS84")))

# Get the map data for Salt Lake County
summit_map <- map("county", "utah,summit", plot = FALSE, fill = TRUE)

# Convert to an sf object
summit_sf <- st_as_sf(map2SpatialPolygons(summit_map, IDs = salt_lake_map$names, 
                                             proj4string = CRS("+proj=longlat +datum=WGS84")))


# Create a basic plot of Utah counties
plot.slcMd <- utah_base + geom_sf(data = new.mask.data, aes(fill = EFm), color = "black")

plot.slcMd <- plot.slcMd +
  geom_sf(data = utah_counties, fill = "transparent", color = "black", linewidth = 0.1) +
  geom_sf(data = salt_lake_sf, fill = NA, color = "#F4C40FFF", linewidth=4) +  # fill = NA removes fill, color = "black" sets outline color
  geom_sf(data = summit_sf, fill = NA, color = "#F4C40FFF", linewidth=4) +  # fill = NA removes fill, color = "black" sets outline color
  geom_sf(data = new.mask.data, fill = "transparent", color = "black", linewidth = 1.25) +
  theme_minimal() 

plot.slcMd


#plot_grid(plot.slcMd, plot.slcMd, plot.slcMd, labels = "AUTO")

colour_breaks <- c(-50, 0, 50, 110)
colours <- c("#67a9cf",
                      "#f7f7f7",
                      "#ef8a62",
                      "#a23a11")
                      
SLSC.plot <- plot.slcMd + 
  #geom_polygon(data = new.mask.data, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #geom_polygon(aes(fill = EFm), color = "white") +
  geom_polygon() +
  theme_bw() +
  ditch_the_axes +
  # scale_fill_gradient2(low = "#67a9cf",
  #                      mid = "#f7f7f7",
  #                      high = "#ef8a62",
  #                      midpoint = 0.0,
  #                      limits = c(-50,50)) +
  scale_fill_gradientn(
    limits  = c(-50, 110),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = c(-50, 110)), 1),
    name = expression("E"["Fm"]))+
  geom_sf(data = i15, aes(color = CARTOCODE), size = 2, show.legend = "line") +
  scale_color_manual(values = "maroon",
                     labels = c("Interstate\nHighways"),
                     name = "Roads")


p.legend2 <- get_legend(SLSC.plot +theme(legend.title=element_text(size=20), 
                                         legend.text=element_text(size=20))) 


## Figure 2a
SLSC.plot2 <- SLSC.plot + theme(legend.position = "none")

# Efm statewide mandate -------------------------------------

udohh %>%
  filter(Date <= as.Date("2021-02-28", "%Y-%m-%d")
         & Date >= as.Date("2020-09-01", "%Y-%m-%d"))  -> smalll.stwdmdt 
## Key Parameters ----------------------------------------------------------

UTMaskMandate <- "2020-11-09" #Date of the state mask mandate

# Filter data by county
#Making datasets for each group
JurisFun <- function(CountySelected){
  smalll.stwdmdt %>% 
    filter(jurisdiction == CountySelected) -> CountyOutput 
  return(CountyOutput)
} 

# Calculate Start and End Case Rate per 100,000 pre mandate 
PreMdtCaseRtFun <- function(county, StartDate, NumDays){
  CaseRtStart <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")- NumDays) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRtEnd <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRt<-c(CaseRtStart, CaseRtEnd)
  return(CaseRt)
}




# Calculate Start and End Case Rate per 100,000 post mandate 

PostMdtCaseRtFun <- function(county, StartDate, NumDays){
  CaseRtStart <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")+1) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRtEnd <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d") + 1 + NumDays) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRt<-c(CaseRtStart, CaseRtEnd)
  return(CaseRt)
}




# Growth rate after regulations 
MaskGrowthFun <- function(mu, sigma, EndCaseRt, StartCaseRt, NumDays){
  rgrow <- (log(EndCaseRt/StartCaseRt)/NumDays) 
  c <- sigma/mu
  c2min <- c^-2
  Rgrow <- (((c^2)*rgrow*mu)+1)^c2min
  return(Rgrow)
}




# Final Calculation
EFmMaskFun <- function(Rmask, Rno){
  EFmMask <- 1-(Rmask/Rno)
  return(EFmMask)
}



### Individual LHD calculation for lifting the mask mandate --------------------

## Salt Lake County
slcc <- JurisFun(CountySelected= "Salt Lake County")
PreMdtCaseRtOutSlcc<-PreMdtCaseRtFun(slcc, UTMaskMandate, NumberDays)
PostMdtCaseRtOutSlcc<-PostMdtCaseRtFun(slcc, UTMaskMandate, NumberDays)
RnoSLC<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutSlcc[2], StartCaseRt= PreMdtCaseRtOutSlcc[1], NumDays  = NumberDays)
RmaskSLC<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutSlcc[2], StartCaseRt= PostMdtCaseRtOutSlcc[1], NumDays  = NumberDays)
EFmMaskSLC <- EFmMaskFun(Rmask = RmaskSLC,Rno = RnoSLC)
EFmMaskSLC*100

EFmMaskSLC
EFmPercentNegSLC <- cbind(EFmMaskSLC*-100, c("Salt Lake County"))
colnames(EFmPercentNegSLC) <- c("EFm", "LHD")

##Summit County problem
sumc <- JurisFun(CountySelected = "Summit County")
PreMdtCaseRtOutsumc<-PreMdtCaseRtFun(sumc, UTMaskMandate, NumberDays)
PostMdtCaseRtOutsumc<-PostMdtCaseRtFun(sumc, UTMaskMandate, NumberDays)
Rnosumc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutsumc[2], StartCaseRt= PreMdtCaseRtOutsumc[1], NumDays  = NumberDays)
Rmasksumc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutsumc[2], StartCaseRt= PostMdtCaseRtOutsumc[1], NumDays  = NumberDays)
EFmMasksumc <- EFmMaskFun(Rmask = Rmasksumc,Rno = Rnosumc)
EFmMasksumc*100

EFmMasksumc
EFmPercentNegSummit <- cbind(EFmMasksumc*-100, c("Summit County"))
colnames(EFmPercentNegSummit) <- c("EFm", "LHD")


## Bear River County(brc)
brc <- JurisFun(CountySelected = "Bear River")
PreMdtCaseRtOutbrc<-PreMdtCaseRtFun(brc, UTMaskMandate, NumberDays)
PostMdtCaseRtOutbrc<-PostMdtCaseRtFun(brc, UTMaskMandate, NumberDays)
Rnobrc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutbrc[2], StartCaseRt= PreMdtCaseRtOutbrc[1], NumDays  = NumberDays)
Rmaskbrc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutbrc[2], StartCaseRt= PostMdtCaseRtOutbrc[1], NumDays  = NumberDays)
EFmMaskbrc <- EFmMaskFun(Rmask = Rmaskbrc,Rno = Rnobrc)
EFmMaskbrc*100

EFmMaskbrc
EFmPercentNegBRC <- cbind(EFmMaskbrc*-100, c("Bear River"))
colnames(EFmPercentNegBRC) <- c("EFm", "LHD")


## Utah County(ucc)
ucc <- JurisFun(CountySelected = "Utah County")
PreMdtCaseRtOutucc<-PreMdtCaseRtFun(ucc, UTMaskMandate, NumberDays)
PostMdtCaseRtOutucc<-PostMdtCaseRtFun(ucc, UTMaskMandate, NumberDays)
Rnoucc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutucc[2], StartCaseRt= PreMdtCaseRtOutucc[1], NumDays  = NumberDays)
Rmaskucc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutucc[2], StartCaseRt= PostMdtCaseRtOutucc[1], NumDays  = NumberDays)
EFmMaskucc <- EFmMaskFun(Rmask = Rmaskucc,Rno = Rnoucc)
EFmMaskucc*100

EFmMaskucc
EFmPercentNegUtah <- cbind(EFmMaskucc*-100, c("Utah County"))
colnames(EFmPercentNegUtah) <- c("EFm", "LHD")

##Davis County(dc)
dc <- JurisFun(CountySelected = "Davis County")
PreMdtCaseRtOutdc<-PreMdtCaseRtFun(dc, UTMaskMandate, NumberDays)
PostMdtCaseRtOutdc<-PostMdtCaseRtFun(dc, UTMaskMandate, NumberDays)
Rnodc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutdc[2], StartCaseRt= PreMdtCaseRtOutdc[1], NumDays  = NumberDays)
Rmaskdc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutdc[2], StartCaseRt= PostMdtCaseRtOutdc[1], NumDays  = NumberDays)
EFmMaskdc <- EFmMaskFun(Rmask = Rmaskdc,Rno = Rnodc)
EFmMaskdc*100

EFmMaskdc
EFmPercentNegDavis <- cbind(EFmMaskdc*-100, c("Davis County"))
colnames(EFmPercentNegDavis) <- c("EFm", "LHD")

##Tooele County(tc) also problematic
tc <- JurisFun(CountySelected = "Tooele County")
PreMdtCaseRtOuttc<-PreMdtCaseRtFun(tc, UTMaskMandate, NumberDays)
PostMdtCaseRtOuttc<-PostMdtCaseRtFun(tc, UTMaskMandate, NumberDays)
Rnotc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOuttc[2], StartCaseRt= PreMdtCaseRtOuttc[1], NumDays  = NumberDays)
Rmasktc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOuttc[2], StartCaseRt= PostMdtCaseRtOuttc[1], NumDays  = NumberDays)
EFmMasktc <- EFmMaskFun(Rmask = Rmasktc,Rno = Rnotc)
EFmMasktc*100

EFmMasktc
EFmPercentNegTooele <- cbind(EFmMasktc*-100, c("Tooele County"))
colnames(EFmPercentNegTooele) <- c("EFm", "LHD")

#Central Utah
cu <- JurisFun(CountySelected = "Central Utah")
PreMdtCaseRtOutcu<-PreMdtCaseRtFun(cu, UTMaskMandate, NumberDays)
PostMdtCaseRtOutcu<-PostMdtCaseRtFun(cu, UTMaskMandate, NumberDays)
Rnocu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutcu[2], StartCaseRt= PreMdtCaseRtOutcu[1], NumDays  = NumberDays)
Rmaskcu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutcu[2], StartCaseRt= PostMdtCaseRtOutcu[1], NumDays  = NumberDays)
EFmMaskcu <- EFmMaskFun(Rmask = Rmaskcu,Rno = Rnocu)
EFmMaskcu*100

EFmMaskcu
EFmPercentNegCentral <- cbind(EFmMaskcu*-100, c("Central Utah"))
colnames(EFmPercentNegCentral) <- c("EFm", "LHD")

#San Juan
sj <- JurisFun(CountySelected = "San Juan")
PreMdtCaseRtOutsj<-PreMdtCaseRtFun(sj, UTMaskMandate, NumberDays)
PostMdtCaseRtOutsj<-PostMdtCaseRtFun(sj, UTMaskMandate, NumberDays)
Rnosj<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutsj[2], StartCaseRt= PreMdtCaseRtOutsj[1], NumDays  = NumberDays)
Rmasksj<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutsj[2], StartCaseRt= PostMdtCaseRtOutsj[1], NumDays  = NumberDays)
EFmMasksj <- EFmMaskFun(Rmask = Rmasksj,Rno = Rnosj)
EFmMasksj*100

EFmMasksj
EFmPercentNegSanJuan <- cbind(EFmMasksj*-100, c("San Juan"))
colnames(EFmPercentNegSanJuan) <- c("EFm", "LHD")


#Southeast Utah
seu <- JurisFun(CountySelected = "Southeast Utah")
PreMdtCaseRtOutseu<-PreMdtCaseRtFun(seu, UTMaskMandate, NumberDays)
PostMdtCaseRtOutseu<-PostMdtCaseRtFun(seu, UTMaskMandate, NumberDays)
Rnoseu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutseu[2], StartCaseRt= PreMdtCaseRtOutseu[1], NumDays  = NumberDays)
Rmaskseu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutseu[2], StartCaseRt= PostMdtCaseRtOutseu[1], NumDays  = NumberDays)
EFmMaskseu <- EFmMaskFun(Rmask = Rmaskseu,Rno = Rnoseu)
EFmMaskseu*100

EFmMaskseu
EFmPercentNegSoutheast <- cbind(EFmMaskseu*-100, c("Southeast Utah"))
colnames(EFmPercentNegSoutheast) <- c("EFm", "LHD")

#Southwest Utah
swu <- JurisFun(CountySelected = "Southwest Utah")
PreMdtCaseRtOutswu<-PreMdtCaseRtFun(swu, UTMaskMandate, NumberDays)
PostMdtCaseRtOutswu<-PostMdtCaseRtFun(swu, UTMaskMandate, NumberDays)
Rnoswu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutswu[2], StartCaseRt= PreMdtCaseRtOutswu[1], NumDays  = NumberDays)
Rmaskswu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutswu[2], StartCaseRt= PostMdtCaseRtOutswu[1], NumDays  = NumberDays)
EFmMaskswu <- EFmMaskFun(Rmask = Rmaskswu,Rno = Rnoswu)
EFmMaskswu*100

EFmMaskswu
EFmPercentNegSouthwest <- cbind(EFmMaskswu*-100, c("Southwest Utah"))
colnames(EFmPercentNegSouthwest) <- c("EFm", "LHD")


#Tricounty
trc <- JurisFun(CountySelected = "TriCounty")
PreMdtCaseRtOuttrc<-PreMdtCaseRtFun(trc, UTMaskMandate, NumberDays)
PostMdtCaseRtOuttrc<-PostMdtCaseRtFun(trc, UTMaskMandate, NumberDays)
Rnotrc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOuttrc[2], StartCaseRt= PreMdtCaseRtOuttrc[1], NumDays  = NumberDays)
Rmasktrc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOuttrc[2], StartCaseRt= PostMdtCaseRtOuttrc[1], NumDays  = NumberDays)
EFmMasktrc <- EFmMaskFun(Rmask = Rmasktrc,Rno = Rnotrc)
EFmMasktrc*100

EFmMasktrc
EFmPercentNegTricounty <- cbind(EFmMasktrc*-100, c("TriCounty"))
colnames(EFmPercentNegTricounty) <- c("EFm", "LHD")



#Weber-Morgan
wmc <- JurisFun(CountySelected = "Weber-Morgan")
PreMdtCaseRtOutwmc<-PreMdtCaseRtFun(wmc, UTMaskMandate, NumberDays)
PostMdtCaseRtOutwmc<-PostMdtCaseRtFun(wmc, UTMaskMandate, NumberDays)
Rnowmc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutwmc[2], StartCaseRt= PreMdtCaseRtOutwmc[1], NumDays  = NumberDays)
Rmaskwmc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutwmc[2], StartCaseRt= PostMdtCaseRtOutwmc[1], NumDays  = NumberDays)
EFmMaskwmc <- EFmMaskFun(Rmask = Rmaskwmc,Rno = Rnowmc)
EFmMaskwmc*100

EFmMaskwmc
EFmPercentNegwebermorgan <- cbind(EFmMaskwmc*-100, c("Weber-Morgan"))
colnames(EFmPercentNegwebermorgan) <- c("EFm", "LHD")

#Wasatch County
wc <- JurisFun(CountySelected = "Wasatch County")
PreMdtCaseRtOutwc<-PreMdtCaseRtFun(wc, UTMaskMandate, NumberDays)
PostMdtCaseRtOutwc<-PostMdtCaseRtFun(wc, UTMaskMandate, NumberDays)
Rnowc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutwc[2], StartCaseRt= PreMdtCaseRtOutwc[1], NumDays  = NumberDays)
Rmaskwc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutwc[2], StartCaseRt= PostMdtCaseRtOutwc[1], NumDays  = NumberDays)
EFmMaskwc <- EFmMaskFun(Rmask = Rmaskwc,Rno = Rnowc)
EFmMaskwc*100

EFmMaskwc
EFmPercentNegwasatch <- cbind(EFmMaskwc*-100, c("Wasatch County"))
colnames(EFmPercentNegwasatch) <- c("EFm", "LHD")

#combining EFm values
Mdt.EFmAll <- as.data.frame(rbind(EFmPercentNegSLC, EFmPercentNegSummit, 
                                      EFmPercentNegwasatch, EFmPercentNegwebermorgan, 
                                      EFmPercentNegTricounty, EFmPercentNegSouthwest, 
                                      EFmPercentNegSoutheast, EFmPercentNegSanJuan, 
                                      EFmPercentNegCentral, EFmPercentNegTooele, 
                                      EFmPercentNegDavis, EFmPercentNegUtah, EFmPercentNegBRC))
Mdt.EFmAll


### Map Making!  ------------------------------------------------------------

merge(Mdt.EFmAll, county, by.x = "LHD", by.y = "LHD", all.x = TRUE, 
      all.y = FALSE) -> Mdt_EFmAll_county


Mdt_EFmAll_countyfinal <- inner_join(Mdt_EFmAll_county, Utah, by=c('County' = 'subregion'))
Mdt_EFmAll_countyfinal$EFm <- as.numeric(Mdt_EFmAll_countyfinal$EFm)


#making map with jurisdiction
write.csv(Mdt_EFmAll_countyfinal, "mdt.Mask.Mandate.csv")
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

mdt.old.mask.data <- read.csv("mdt.Mask.Mandate.csv")

## ALICIA: You can either repeat this step 3 times, or merge 
mdt.old.mask.data <- mdt.old.mask.data %>% 
  dplyr::select("LHD", "EFm") %>%
  group_by(LHD) %>% 
  slice_head(n = 1)

mdt.new.mask.data <-merge(health_districts, mdt.old.mask.data, by.x = c("DISTNAME"), 
                               by.y = c("LHD"), all.x = TRUE, all.y = FALSE)


# # Get the Utah county boundaries from the maps package
# utah_counties <- map("county", "utah", plot = FALSE, fill = TRUE)

# # Convert the county boundaries to an sf object
# utah_counties <- st_as_sf(utah_counties)


# Get the map data for Salt Lake County
state_map <- map("state", "utah", plot = FALSE, fill = TRUE)

# Convert to an sf object
state_sf <- st_as_sf(map2SpatialPolygons(state_map, IDs = salt_lake_map$names, 
                                          proj4string = CRS("+proj=longlat +datum=WGS84")))





# Create a basic plot of Utah counties
plot.mdt <- utah_base + geom_sf(data = mdt.new.mask.data, aes(fill = EFm), color = "black")

plot.mdt <- plot.mdt +
  geom_sf(data = utah_counties, fill = "transparent", color = "black", linewidth = 0.1) +
  geom_sf(data = state_sf, fill = NA, color = "#F4C40FFF", linewidth=4) +  # fill = NA removes fill, color = "black" sets outline color
  geom_sf(data = new.mask.data, fill = "transparent", color = "black", linewidth = 1.25) +
  theme_minimal() 

plot.mdt

      
plot.mdt <- plot.mdt + 
  #geom_polygon(data = new.mask.data, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #geom_polygon(aes(fill = EFm), color = "white") +
  geom_polygon(color = "grey", fill = NA) +
  theme_bw() +
  ditch_the_axes +
  # scale_fill_gradient2(low = "#67a9cf",
  #                      mid = "#f7f7f7",
  #                      high = "#ef8a62",
  #                      midpoint = 0.0,
  #                      limits = c(-50,50)) + 
  scale_fill_gradientn(
    limits  = c(-50, 105),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = c(-50, 105)), 1),
    name = expression("E"["Fm"]))+
  geom_sf(data = i15, aes(color = CARTOCODE), size = 2, show.legend = "line") +
  scale_color_manual(values = "maroon",
                     labels = c("Interstate Highways"),
                     name = "Roads")


## Figure 2b
Mdt.plot2 <- plot.mdt + 
  theme(legend.position = "none")




# Efm lifting statewide mandate -------------------------------------

udohh %>% 
  filter(Date <= as.Date("2021-06-28", "%Y-%m-%d")
         & Date >= as.Date("2021-03-01", "%Y-%m-%d")) -> smalll.postmdt 
## Key Parameters ----------------------------------------------------------

SLCMaskMandateEnd <- "2021-04-10" #Date of the SLC mask mandate


# Filter data by county
#Making datasets for each group
JurisFun <- function(CountySelected){
  smalll.postmdt %>% 
    filter(jurisdiction == CountySelected) -> CountyOutput 
  return(CountyOutput)
} 

# Calculate Start and End Case Rate per 100,000 pre mandate 
PreMdtCaseRtFun <- function(county, StartDate, NumDays){
  CaseRtStart <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")- NumDays) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRtEnd <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRt<-c(CaseRtStart, CaseRtEnd)
  return(CaseRt)
}




# Calculate Start and End Case Rate per 100,000 post mandate 

PostMdtCaseRtFun <- function(county, StartDate, NumDays){
  CaseRtStart <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d")+1) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRtEnd <- county %>%
    filter(Date == as.Date(StartDate, "%Y-%m-%d") + 1 + NumDays) %>%
    pull(Seven.day.Average.Case.Rate)
  CaseRt<-c(CaseRtStart, CaseRtEnd)
  return(CaseRt)
}




# Growth rate after regulations 
MaskGrowthFun <- function(mu, sigma, EndCaseRt, StartCaseRt, NumDays){
  rgrow <- (log(EndCaseRt/StartCaseRt)/NumDays) 
  c <- sigma/mu
  c2min <- c^-2
  Rgrow <- (((c^2)*rgrow*mu)+1)^c2min
  return(Rgrow)
}




# Final Calculation
EFmMaskFun <- function(Rmask, Rno){
  EFmMask <- 1-(Rmask/Rno)
  return(EFmMask)
}



### Individual LHD calculation for lifting the mask mandate --------------------

## Salt Lake County
slcc <- JurisFun(CountySelected= "Salt Lake County")
PreMdtCaseRtOutSlcc<-PreMdtCaseRtFun(slcc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutSlcc<-PostMdtCaseRtFun(slcc, SLCMaskMandateEnd, NumberDays)
RnoSLC<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutSlcc[2], StartCaseRt= PreMdtCaseRtOutSlcc[1], NumDays  = NumberDays)
RmaskSLC<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutSlcc[2], StartCaseRt= PostMdtCaseRtOutSlcc[1], NumDays  = NumberDays)
EFmMaskSLC <- EFmMaskFun(Rmask = RmaskSLC,Rno = RnoSLC)
EFmMaskSLC*100

EFmMaskSLC
EFmPercentNegSLC <- cbind(EFmMaskSLC*-100, c("Salt Lake County"))
colnames(EFmPercentNegSLC) <- c("EFm", "LHD")

##Summit County problem
sumc <- JurisFun(CountySelected = "Summit County")
PreMdtCaseRtOutsumc<-PreMdtCaseRtFun(sumc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutsumc<-PostMdtCaseRtFun(sumc, SLCMaskMandateEnd, NumberDays)
Rnosumc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutsumc[2], StartCaseRt= PreMdtCaseRtOutsumc[1], NumDays  = NumberDays)
Rmasksumc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutsumc[2], StartCaseRt= PostMdtCaseRtOutsumc[1], NumDays  = NumberDays)
EFmMasksumc <- EFmMaskFun(Rmask = Rmasksumc,Rno = Rnosumc)
EFmMasksumc*100

EFmMasksumc
EFmPercentNegSummit <- cbind(EFmMasksumc*-100, c("Summit County"))
colnames(EFmPercentNegSummit) <- c("EFm", "LHD")


## Bear River County(brc)
brc <- JurisFun(CountySelected = "Bear River")
PreMdtCaseRtOutbrc<-PreMdtCaseRtFun(brc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutbrc<-PostMdtCaseRtFun(brc, SLCMaskMandateEnd, NumberDays)
Rnobrc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutbrc[2], StartCaseRt= PreMdtCaseRtOutbrc[1], NumDays  = NumberDays)
Rmaskbrc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutbrc[2], StartCaseRt= PostMdtCaseRtOutbrc[1], NumDays  = NumberDays)
EFmMaskbrc <- EFmMaskFun(Rmask = Rmaskbrc,Rno = Rnobrc)
EFmMaskbrc*100

EFmMaskbrc
EFmPercentNegBRC <- cbind(EFmMaskbrc*-100, c("Bear River"))
colnames(EFmPercentNegBRC) <- c("EFm", "LHD")


## Utah County(ucc)
ucc <- JurisFun(CountySelected = "Utah County")
PreMdtCaseRtOutucc<-PreMdtCaseRtFun(ucc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutucc<-PostMdtCaseRtFun(ucc, SLCMaskMandateEnd, NumberDays)
Rnoucc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutucc[2], StartCaseRt= PreMdtCaseRtOutucc[1], NumDays  = NumberDays)
Rmaskucc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutucc[2], StartCaseRt= PostMdtCaseRtOutucc[1], NumDays  = NumberDays)
EFmMaskucc <- EFmMaskFun(Rmask = Rmaskucc,Rno = Rnoucc)
EFmMaskucc*100

EFmMaskucc
EFmPercentNegUtah <- cbind(EFmMaskucc*-100, c("Utah County"))
colnames(EFmPercentNegUtah) <- c("EFm", "LHD")

##Davis County(dc)
dc <- JurisFun(CountySelected = "Davis County")
PreMdtCaseRtOutdc<-PreMdtCaseRtFun(dc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutdc<-PostMdtCaseRtFun(dc, SLCMaskMandateEnd, NumberDays)
Rnodc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutdc[2], StartCaseRt= PreMdtCaseRtOutdc[1], NumDays  = NumberDays)
Rmaskdc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutdc[2], StartCaseRt= PostMdtCaseRtOutdc[1], NumDays  = NumberDays)
EFmMaskdc <- EFmMaskFun(Rmask = Rmaskdc,Rno = Rnodc)
EFmMaskdc*100

EFmMaskdc
EFmPercentNegDavis <- cbind(EFmMaskdc*-100, c("Davis County"))
colnames(EFmPercentNegDavis) <- c("EFm", "LHD")

##Tooele County(tc) also problematic
tc <- JurisFun(CountySelected = "Tooele County")
PreMdtCaseRtOuttc<-PreMdtCaseRtFun(tc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOuttc<-PostMdtCaseRtFun(tc, SLCMaskMandateEnd, NumberDays)
Rnotc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOuttc[2], StartCaseRt= PreMdtCaseRtOuttc[1], NumDays  = NumberDays)
Rmasktc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOuttc[2], StartCaseRt= PostMdtCaseRtOuttc[1], NumDays  = NumberDays)
EFmMasktc <- EFmMaskFun(Rmask = Rmasktc,Rno = Rnotc)
EFmMasktc*100

EFmMasktc
EFmPercentNegTooele <- cbind(EFmMasktc*-100, c("Tooele County"))
colnames(EFmPercentNegTooele) <- c("EFm", "LHD")

#Central Utah
cu <- JurisFun(CountySelected = "Central Utah")
PreMdtCaseRtOutcu<-PreMdtCaseRtFun(cu, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutcu<-PostMdtCaseRtFun(cu, SLCMaskMandateEnd, NumberDays)
Rnocu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutcu[2], StartCaseRt= PreMdtCaseRtOutcu[1], NumDays  = NumberDays)
Rmaskcu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutcu[2], StartCaseRt= PostMdtCaseRtOutcu[1], NumDays  = NumberDays)
EFmMaskcu <- EFmMaskFun(Rmask = Rmaskcu,Rno = Rnocu)
EFmMaskcu*100

EFmMaskcu
EFmPercentNegCentral <- cbind(EFmMaskcu*-100, c("Central Utah"))
colnames(EFmPercentNegCentral) <- c("EFm", "LHD")

#San Juan
sj <- JurisFun(CountySelected = "San Juan")
PreMdtCaseRtOutsj<-PreMdtCaseRtFun(sj, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutsj<-PostMdtCaseRtFun(sj, SLCMaskMandateEnd, NumberDays)
Rnosj<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutsj[2], StartCaseRt= PreMdtCaseRtOutsj[1], NumDays  = NumberDays)
Rmasksj<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutsj[2], StartCaseRt= PostMdtCaseRtOutsj[1], NumDays  = NumberDays)
EFmMasksj <- EFmMaskFun(Rmask = Rmasksj,Rno = Rnosj)
EFmMasksj*100

EFmMasksj
EFmPercentNegSanJuan <- cbind(EFmMasksj*-100, c("San Juan"))
colnames(EFmPercentNegSanJuan) <- c("EFm", "LHD")


#Southeast Utah
seu <- JurisFun(CountySelected = "Southeast Utah")
PreMdtCaseRtOutseu<-PreMdtCaseRtFun(seu, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutseu<-PostMdtCaseRtFun(seu, SLCMaskMandateEnd, NumberDays)
Rnoseu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutseu[2], StartCaseRt= PreMdtCaseRtOutseu[1], NumDays  = NumberDays)
Rmaskseu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutseu[2], StartCaseRt= PostMdtCaseRtOutseu[1], NumDays  = NumberDays)
EFmMaskseu <- EFmMaskFun(Rmask = Rmaskseu,Rno = Rnoseu)
EFmMaskseu*100

EFmMaskseu
EFmPercentNegSoutheast <- cbind(EFmMaskseu*-100, c("Southeast Utah"))
colnames(EFmPercentNegSoutheast) <- c("EFm", "LHD")

#Southwest Utah
swu <- JurisFun(CountySelected = "Southwest Utah")
PreMdtCaseRtOutswu<-PreMdtCaseRtFun(swu, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutswu<-PostMdtCaseRtFun(swu, SLCMaskMandateEnd, NumberDays)
Rnoswu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutswu[2], StartCaseRt= PreMdtCaseRtOutswu[1], NumDays  = NumberDays)
Rmaskswu<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutswu[2], StartCaseRt= PostMdtCaseRtOutswu[1], NumDays  = NumberDays)
EFmMaskswu <- EFmMaskFun(Rmask = Rmaskswu,Rno = Rnoswu)
EFmMaskswu*100

EFmMaskswu
EFmPercentNegSouthwest <- cbind(EFmMaskswu*-100, c("Southwest Utah"))
colnames(EFmPercentNegSouthwest) <- c("EFm", "LHD")


#Tricounty
trc <- JurisFun(CountySelected = "TriCounty")
PreMdtCaseRtOuttrc<-PreMdtCaseRtFun(trc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOuttrc<-PostMdtCaseRtFun(trc, SLCMaskMandateEnd, NumberDays)
Rnotrc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOuttrc[2], StartCaseRt= PreMdtCaseRtOuttrc[1], NumDays  = NumberDays)
Rmasktrc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOuttrc[2], StartCaseRt= PostMdtCaseRtOuttrc[1], NumDays  = NumberDays)
EFmMasktrc <- EFmMaskFun(Rmask = Rmasktrc,Rno = Rnotrc)
EFmMasktrc*100

EFmMasktrc
EFmPercentNegTricounty <- cbind(EFmMasktrc*-100, c("TriCounty"))
colnames(EFmPercentNegTricounty) <- c("EFm", "LHD")



#Weber-Morgan
wmc <- JurisFun(CountySelected = "Weber-Morgan")
PreMdtCaseRtOutwmc<-PreMdtCaseRtFun(wmc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutwmc<-PostMdtCaseRtFun(wmc, SLCMaskMandateEnd, NumberDays)
Rnowmc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutwmc[2], StartCaseRt= PreMdtCaseRtOutwmc[1], NumDays  = NumberDays)
Rmaskwmc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutwmc[2], StartCaseRt= PostMdtCaseRtOutwmc[1], NumDays  = NumberDays)
EFmMaskwmc <- EFmMaskFun(Rmask = Rmaskwmc,Rno = Rnowmc)
EFmMaskwmc*100

EFmMaskwmc
EFmPercentNegwebermorgan <- cbind(EFmMaskwmc*-100, c("Weber-Morgan"))
colnames(EFmPercentNegwebermorgan) <- c("EFm", "LHD")

#Wasatch County
wc <- JurisFun(CountySelected = "Wasatch County")
PreMdtCaseRtOutwc<-PreMdtCaseRtFun(wc, SLCMaskMandateEnd, NumberDays)
PostMdtCaseRtOutwc<-PostMdtCaseRtFun(wc, SLCMaskMandateEnd, NumberDays)
Rnowc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PreMdtCaseRtOutwc[2], StartCaseRt= PreMdtCaseRtOutwc[1], NumDays  = NumberDays)
Rmaskwc<-MaskGrowthFun(mu = mean.gt, sigma  = stdev.gt, EndCaseRt = PostMdtCaseRtOutwc[2], StartCaseRt= PostMdtCaseRtOutwc[1], NumDays  = NumberDays)
EFmMaskwc <- EFmMaskFun(Rmask = Rmaskwc,Rno = Rnowc)
EFmMaskwc*100

EFmMaskwc
EFmPercentNegwasatch <- cbind(EFmMaskwc*-100, c("Wasatch County"))
colnames(EFmPercentNegwasatch) <- c("EFm", "LHD")

#combining EFm values
PostMdt.EFmAll <- as.data.frame(rbind(EFmPercentNegSLC, EFmPercentNegSummit, 
                                      EFmPercentNegwasatch, EFmPercentNegwebermorgan, 
                                      EFmPercentNegTricounty, EFmPercentNegSouthwest, 
                                      EFmPercentNegSoutheast, EFmPercentNegSanJuan, 
                                      EFmPercentNegCentral, EFmPercentNegTooele, 
                                      EFmPercentNegDavis, EFmPercentNegUtah, EFmPercentNegBRC))
PostMdt.EFmAll


### Map Making!  ------------------------------------------------------------

merge(PostMdt.EFmAll, county, by.x = "LHD", by.y = "LHD", all.x = TRUE,
      all.y = FALSE) -> PostMdt_EFmAll_county


PostMdt_Utah_final <- inner_join(PostMdt_EFmAll_county, Utah, by=c('County' = 'subregion'))
PostMdt_Utah_final$EFm <- as.numeric(PostMdt_Utah_final$EFm)


#making map with jurisdiction
write.csv(PostMdt_Utah_final, "Post.mdt.Mask.Mandate.csv")
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

post.mdt.old.mask.data <- read.csv("Post.mdt.Mask.Mandate.csv")

## ALICIA: You can either repeat this step 3 times, or merge 
post.mdt.old.mask.data <- post.mdt.old.mask.data %>% 
  dplyr::select("LHD", "EFm") %>%
  group_by(LHD) %>% 
  slice_head(n = 1)

post.mdt.new.mask.data <-merge(health_districts, post.mdt.old.mask.data, by.x = c("DISTNAME"), 
                               by.y = c("LHD"), all.x = TRUE, all.y = FALSE)


# 
# # Get the Utah county boundaries from the maps package
# utah_counties <- map("county", "utah", plot = FALSE, fill = TRUE)
# 
# # Convert the county boundaries to an sf object
# utah_counties <- st_as_sf(utah_counties)


# Get the map data for Salt Lake County
grand_map <- map("county", "utah,grand", plot = FALSE, fill = TRUE)

# Convert to an sf object
grand_sf <- st_as_sf(map2SpatialPolygons(grand_map, IDs = salt_lake_map$names, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84")))


# Coordinates for Salt Lake City
salt_lake_city <- data.frame(
  city = "Salt Lake City",
  lat = 40.7608,
  long = -111.8910
)

# Convert the coordinates to an sf object
salt_lake_city_sf <- st_as_sf(salt_lake_city, coords = c("long", "lat"), crs = 4326)


# Create a basic plot of Utah counties
plot.post.mdt <- utah_base + geom_sf(data = post.mdt.new.mask.data, aes(fill = EFm), color = "black")

plot.post.mdt <- plot.post.mdt +
  geom_sf(data = grand_sf, fill = NA, color = "#F4C40FFF", linewidth=4) +  # fill = NA removes fill, color = "black" sets outline color
  geom_sf(data = utah_counties, fill = "transparent", color = "black", linewidth = 0.1) +
  geom_sf(data = new.mask.data, fill = "transparent", color = "black", linewidth = 1.25) +
  geom_sf(data = salt_lake_city_sf, color = "#F4C40FFF", size = 10) +  # Point for Salt Lake City
  theme_minimal() 

plot.post.mdt



colour_breaks <- c(-50, 0, 50, 105)
colours <- c("#67a9cf",
                      "#f7f7f7",
                      "#ef8a62",
                      "#a23a11")
                      
plot.post.mdt <- plot.post.mdt + 
  #geom_polygon(data = new.mask.data, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  #geom_polygon(aes(fill = EFm), color = "white") +
  geom_polygon(color = "grey", fill = NA) +
  theme_bw() +
  ditch_the_axes +
  # scale_fill_gradient2(low = "#67a9cf",
  #                      mid = "#f7f7f7",
  #                      high = "#ef8a62",
  #                      midpoint = 0.0,
  #                      limits = c(-50,50)) + 
  scale_fill_gradientn(
    limits  = c(-50, 105),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = c(-50, 105)), 1),
    name = expression("E"["Fm"]))+
  geom_sf(data = i15, aes(color = CARTOCODE), size = 2, show.legend = "line") +
  scale_color_manual(values = "maroon",
                     labels = c("Interstate\nHighways"),
                     name = "Roads")


## Figure 2c
PostMdt.plot2 <- plot.post.mdt + 
  theme(legend.position = "none")



# ## Make one Figure 2 ----------------------------------------------------


Fig2 <- plot_grid(SLSC.plot2, Mdt.plot2, PostMdt.plot2, p.legend2, nrow = 1, 
                  rel_widths = c(1,1,1,0.3), labels = c("A", "B", "C", " "))





# Labeled Map of LHDs -----------------------------------------------------

hd_dat <- as.data.frame(sf::st_coordinates(health_districts))

hd_dat <- hd_dat %>%
  group_by(L2) %>% 
  slice_head(n = 1)

hd_dat$Name <- health_districts$DISTNAME

## Edit where I want the labels to show up
## Southwest
hd_dat[1,2] <- 37.75
hd_dat[1,5] <- "Southwest"

## Central
hd_dat[2,1] <- -112.7
hd_dat[2,2] <- 39.1
hd_dat[2,5] <- "Central"

## Southeast
hd_dat[3,1] <- -110.1
hd_dat[3,2] <- 39
hd_dat[3,5] <- "Southeast"

## Utah
hd_dat[4,1] <- -111.7
hd_dat[4,2] <- 40.2
hd_dat[4,5] <- "Utah"

## Bear River
hd_dat[5,1] <- -113
hd_dat[5,2] <- 41.5

## Summit County
hd_dat[6,1] <- -110.9
hd_dat[6,2] <- 40.9
hd_dat[6,5] <- "Summit"

## TriCounty
hd_dat[7,1] <- -109.9
hd_dat[7,2] <- 40.3

## Tooele
hd_dat[8,1] <- -113.1
hd_dat[8,2] <- 40.5
hd_dat[8,5] <- "Tooele"

## Wasatch
hd_dat[10,1] <- -111.25
hd_dat[10,2] <- 40.499
hd_dat[10,5] <- "Wasatch"

## Davis
hd_dat[11,2] <- 41
hd_dat[11,5] <- "Davis"

## Salt Lake
hd_dat[12,2] <- 40.75
hd_dat[12,5] <- "Salt"

hd_dat[15,1] <- -111.8
hd_dat[15,2] <- 40.65
hd_dat[15,3] <- 1
hd_dat[15,4] <- 12
hd_dat[15,5] <- "Lake"


## San Juan
hd_dat[13,1] <- -110
hd_dat[13,2] <- 37.5
hd_dat[13,5] <- "San Juan"

## Weeber-Morgan
hd_dat[9,1] <- -111.8
hd_dat[9,2] <- 41.3
hd_dat[9,5] <- "Weeber-"

## Weeber-Morgan
hd_dat[14,1] <- -111.555
hd_dat[14,2] <- 41.2
hd_dat[14,3] <- 1
hd_dat[14,4] <- 9
hd_dat[14,5] <- "Morgan"

empty.map <- ggplot() + 
  geom_sf(data = health_districts, aes(fill = DISTNAME), color = "black", alpha = 0.9) +
  geom_sf(data = utah_counties, fill = "transparent", color = "#EEEEE4", linewidth = 0.05) +
  geom_sf(data = health_districts, color = "black", fill = "transparent", linewidth = 1.2) +
  geom_text(data = hd_dat, aes(X, Y, label = Name), colour = "white") + 
  theme_bw() +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#206e9c", "#2b9b81", "#633372", "#9f5691", "#aa7aa1",
                                        "#e6a2a6", "#e87b88", "#9b3441","#d8443d",
                                        "#fe9b01", "#f4c40e", "#91c050", "#de587c")) +
  ditch_the_axes 
  
empty.map


### Vaccination Data -----
vaccines <- read.csv("~/Downloads/Utah_COVID19_data/Vaccine_Cumulative vaccine administrations deliveries_2024-07-24.csv")
summary(vaccines)
#converting date
vaccines$vaccine_date <- lubridate::mdy(vaccines$vaccine_date)
View(vaccines)

vaccines %>% 
  filter(vaccine_date <= as.Date("2021-06-28")) %>% 
  filter(vaccine_date >= as.Date("2020-12-10")) -> smallvaccine 
#udohh <- group_by(as.Date())
vaccineplot <- ggplot(smallvaccine) +
  geom_line(aes(x = vaccine_date, y = Cumulative.People.Received.at.Least.One.Dose)) +
  xlab("Date of Vaccine Dose") +
  ylab("Cumulative number of people recieving at least one dose")
vaccineplot



# Supplemental changing Gt analysis -----------------
# Calculate the total number of cases per week
udoh.variants.filtered.total <- udoh.variants.filtered %>%
  group_by(Week.Sample.was.Collected) %>%
  summarize(Total_Identified = sum(Number.Identified.in.Utah, na.rm = TRUE), .groups = 'drop')

# Merge the totals back into the original dataframe
udoh.variants.filtered <- udoh.variants.filtered %>%
  left_join(udoh.variants.filtered.total, by = "Week.Sample.was.Collected")

# Calculate the percentage for each lineage within each week
udoh.variants.filtered <- udoh.variants.filtered %>%
  mutate(Percentage = (Number.Identified.in.Utah / Total_Identified) * 100)



percents<-udoh.variants.filtered %>%
  group_by(Week.Sample.was.Collected) %>%
  summarize(Total_percent = sum(Percentage, na.rm = TRUE), .groups = 'drop')

udoh.variants.filtered <- udoh.variants.filtered %>%
  mutate(Lineage = factor(Lineage, levels = unique(Lineage)))


ggplot(udoh.variants.filtered, aes(x = Week.Sample.was.Collected, y = Percentage, fill = Lineage)) + 
  geom_bar(position="stack", stat="identity", width = 7) +
  xlab("Week Sample Collected") + 
  ylab("% Identified ") + 
  theme_bw() + 
  scale_fill_discrete(
    labels = c("B.1.1.7" = "Alpha", "B.1.351" = "Beta", "B.1.427" = "Epsilon1",
               "B.1.429" = "Epsilon2", "B.1.617.2" = "Delta",
               "Other Lineage" = "Other\nLineage", "P.1" = "Gamma", "B.1.1.529" = "Omicron")) +
  theme(legend.position = "bottom")+
  geom_vline(xintercept = as.Date("2020-06-28", "%Y-%m-%d")) + 
  annotate("text", x= (as.Date("2020-07-10", "%Y-%m-%d")+1), y = 40, label = "SLSC Mandate", angle=270, hjust = 0, size = 3.5) +
  geom_vline(xintercept = as.Date("2020-11-09", "%Y-%m-%d")) +
  annotate("text", x= (as.Date("2020-11-20", "%Y-%m-%d")+1), y = 49, label = "Statewide Mandate", angle=270, hjust = 0, size = 3.5) +
  geom_vline(xintercept = as.Date("2021-04-10", "%Y-%m-%d")) +
  annotate("text", x= (as.Date("2021-04-20", "%Y-%m-%d")+1), y = 62, label = "Statewide Mandate Lifted", angle=270, hjust = 0, size = 3.5) + 
  xlim(as_date("2020-06-01"), as_date("2021-05-09"))


