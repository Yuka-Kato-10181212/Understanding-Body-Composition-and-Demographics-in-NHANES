#Data Preparation for Body Composition and Demographics 




#This file does not include the code to determine the phenotypes for individual participants.
#It should be calculated by referencing on 
#Prado et al. (2014) "A population-based approach to define body-composition phenotypes"
#https://doi.org/10.3945/ajcn.113.078576



############################################################################################
#install.packages('apyramid')
#install.packages('ggplot2')
#install.packages('readr')
#install.packages('patchwork')


require('haven')
require('srvyr')
require('survey')
require('dplyr')
require('tidyr')
require('apyramid')
require('ggplot2')
require('readr')
require('patchwork')


path <- "" #Add your path to the folder storing the data

#Uploading the necessary data files 
demo1112 <- read_xpt(paste0(path, '\\DEMO_G.XPT'))
demo1314 <- read_xpt(paste0(path, '\\DEMO_H.XPT'))
demo1516 <- read_xpt(paste0(path, '\\DEMO_I.XPT'))
demo1718 <- read_xpt(paste0(path, '\\DEMO_J.XPT'))

df1112 <- read_xpt(paste0(path, '\\DXX_G.XPT'))
df1314 <- read_xpt(paste0(path, '\\DXX_H.XPT'))
df1516 <- read_xpt(paste0(path, '\\DXX_I.XPT'))
df1718 <- read_xpt(paste0(path, '\\DXX_J.XPT'))

bm1112 <- read_xpt(paste0(path, '\\BMX_G.XPT'))
bm1314 <- read_xpt(paste0(path, '\\BMX_H.XPT'))
bm1516 <- read_xpt(paste0(path, '\\BMX_I.XPT'))
bm1718 <- read_xpt(paste0(path, '\\BMX_J.XPT'))

phenotype <- read.csv(paste0('path\\nhanes_phenotype.csv'))

#Check variables
names(demo1112)


#Merge df of each year to the demo 
all1112 <- demo1112 %>% left_join(bm1112, by = 'SEQN') %>% left_join(df1112, by = 'SEQN')
all1314 <- demo1314 %>% left_join(bm1314, by = 'SEQN') %>% left_join(df1314, by = 'SEQN')
all1516 <- demo1516 %>% left_join(bm1516, by = 'SEQN') %>% left_join(df1516, by = 'SEQN')
all1718 <- demo1718 %>% left_join(bm1718, by = 'SEQN') %>% left_join(df1718, by = 'SEQN')


#Concatenate all the merged data frames above 
df <- bind_rows(all1112, all1314,all1516, all1718)

#Check if there is a missing SEQN
setdiff(df$SEQN, phenotype$SEQN)
setdiff(phenotype$SEQN, df$SEQN)


#Delete the duplicated column
df <- subset(df, select = -(SDDSRVYR.y)) %>% rename('SDDSRVYR' = 'SDDSRVYR.x')
names(df)

#Calculate the 8 years weight by following 
#https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx

table(df$SDDSRVYR)
df <- df %>% mutate(MEC8YR = 1/4 * WTMEC2YR) 


################################################################################################
#Trimming the sample 
#Change the variable names for simplicity 
df <- df %>% rename(weight = BMXWT,
                    height = BMXHT,
                    age = RIDAGEYR, #1 = male
                    race1 = RIDRETH1,
                    race2 = RIDRETH3,
                    gender = RIAGENDR,
                    maritalS = DMDMARTL,
                    preg = RIDEXPRG,
                    education = DMDEDUC2,
                    bmi = BMXBMI,
                    pir = INDFMPIR,
                    hh_income = INDHHIN2)




#Since RIDEXPRG(pregnancy) is categorical var. filter function drops NA automatically, 
#add 0 to NA, for height and weight, replace NA to 1000
table(df$preg)
df$preg[is.na(df$preg)] <- 0

#category 1 and 2 are Hispanics, change 2 to 1 to indicate bothe are hispanic
df$race1[df$race1 == 2] <- 1
df$race1[df$race1 == 3] <- 2
df$race1[df$race1 == 4] <- 3
df$race1[df$race1 == 5] <- 4

#Change marital status 
df$maritalS[df$maritalS == 5] <-"Single"
df$maritalS[df$maritalS == 1] <-"Married/ Partner"
df$maritalS[df$maritalS == 6] <-"Married/ Partner"
df$maritalS[df$maritalS == 2] <-"Prev. Married"
df$maritalS[df$maritalS == 3] <-"Prev. Married"
df$maritalS[df$maritalS == 4] <-"Prev. Married"

#Change education status
#1: Less than Highschool
#2: Highschool
#3: College or above

df <- df %>%
  mutate(education = ifelse(education %in% c(1, 2), 1,
                            ifelse(education %in% c(4, 5), "3", "2")),
         education = factor(education),
         pir2 = ifelse(pir<1.3, "PIR < 1.3", "PIR >= 1.3")) %>% 
  filter(RIDSTATR ==2,
                    age >= 20,
                    preg != 1,
                    weight <= 136,
                    height <= 196,
                    maritalS != '',
                    DXAEXSTS == 1,
                    maritalS != 77 | maritalS != 99,
                    phenotype != '')

#df for male 
df_m <- subset(df, gender == 1)


#addf: adjusted df

addf <- df %>% 
  as_survey_design(ids = SDMVPSU, 
                   weights = MEC8YR, 
                   strata = SDMVSTRA,
                   nest = T) %>%
  mutate(gender = factor(gender, levels = c(1, 2)),
                        phenotype = factor(phenotype, 
                                           levels = c('HA_HM', 'HA_LM', 'LA_HM', 'LA_LM')),
                        education = factor(education, 
                                           levels = c('1', '2','3')),
                        race1 = factor(race1, levels = c('1', '2', '3', '4')),
                        maritalS = factor(maritalS,  
                                          levels = c('Single', "Married/ Partner", "Prev. Married")))


##########################################################################################

#Divide the addf by phenotypes
adhahm <- subset(addf, phenotype == "HA_HM")
adhalm <- subset(addf, phenotype == "HA_LM")
adlahm <- subset(addf, phenotype == "LA_HM")
adlalm <- subset(addf, phenotype == "LA_LM")


#stratify by gender
m_addf <- subset(addf, gender == 1)
f_addf <- subset(addf, gender == 2)

m_hahm <- subset(m_addf, phenotype == "HA_HM")
m_halm <- subset(m_addf, phenotype == "HA_LM")
m_lahm <- subset(m_addf, phenotype == "LA_HM")
m_lalm <- subset(m_addf, phenotype == "LA_LM")

f_hahm <- subset(f_addf, phenotype == "HA_HM")
f_halm <- subset(f_addf, phenotype == 'HA_LM')
f_lahm <- subset(f_addf, phenotype == "LA_HM")
f_lalm <- subset(f_addf, phenotype == "LA_LM")


###################################################################################################
###### For scatter plot

cat <- df %>% select(c('SEQN', 'SDMVPSU', 'MEC8YR', 'SDMVSTRA', 'phenotype',
                       'FMI', 'ASMI', 'DECILE_fmi', 'DECILE_asmi')) %>% 
  mutate(FMI = round(FMI, 1),
         ASMI = round(ASMI, 1))

adcat <- cat %>% as_survey_design(ids = SDMVPSU, 
                                  weights = MEC8YR, 
                                  strata = SDMVSTRA,
                                  nest = T) 


# Create separate datasets for each category
cat_hahm <- subset(adcat, phenotype == "HA_HM")
cat_halm <- subset(adcat, phenotype == "HA_LM")
cat_lahm <- subset(adcat, phenotype == "LA_HM")
cat_lalm <- subset(adcat, phenotype == "LA_LM")


################################################################################################
###### For the circular stacked barcharts
#since making a circle requires a lot of bars, make the age group in 5 years gaps

circ_r1 <- addf %>% select(c('SEQN', 'age', 'gender', 'race1', 'phenotype'))
circ1 <- as.data.frame(svytable(~gender + race1 + age + phenotype , 
                                circ_r1))


circ1 <- circ1 %>%
  mutate(age = as.numeric(as.character(age)),  # Convert age to numeric
         age_group = case_when(
           age >= 20 & age < 30 ~ "20-29",
           age >= 30 & age < 40 ~ "30-39",
           age >= 40 & age < 50 ~ "40-49",
           age >= 50 & age <= 59 ~ "50-59",
         ),
         age_group2 = case_when(
           age >= 20 & age < 25 ~ "20-24",
           age >= 25 & age < 30 ~ "25-29",
           age >= 30 & age < 35 ~ "30-34",
           age >= 35 & age < 40 ~ "35-39",
           age >= 40 & age < 45 ~ "40-44",
           age >= 45 & age < 50 ~ "45-49",
           age >= 50 & age < 55 ~ "50-54",
           age >= 55 & age <= 59 ~ "55-59",
         ))

# Convert age_group and age_group2 to factors
circ1$age_group <- factor(circ1$age_group, levels = c("20-29", "30-39", "40-49", "50-59"))
circ1$age_group2 <- factor(circ1$age_group2, levels = c("20-24", "25-29", "30-34", "35-39", 
                                                        "40-44", "45-49", "50-54", "55-59"))



circ1 <- circ1 %>% mutate(race_char = case_when(
  race1 == 1 ~ 'Hispanic',
  race1 == 2 ~ 'White',
  race1 == 3 ~ 'Black',
  race1 == 4 ~ 'Other',
  TRUE ~ as.character(race1)  # Default case, converts race to character
))




m_circ1 <- circ1 %>% filter(gender == 1) %>% select(-gender)




#####################################################################################
##Functions

get_hist_data <- function(data, var, breaks = 10) {
  hist_result <- svyhist(as.formula(paste0("~", var)), data, breaks = breaks)
  density_str <- hist_result$density
  density_numeric <- as.numeric(gsub(".*\\[|\\].*", "", density_str))
  breaks <- hist_result$breaks
  df <- data.frame(breaks = breaks[-length(breaks)], density = density_numeric)
  return(df)
}

get_mean <- function(data, value_var) {
  result <- data %>%
    group_by(phenotype) %>%
    summarise(mean = survey_mean(!!as.name(value_var), na.rm = TRUE, vartype = "ci")) %>%
    pull(mean) %>%
    round(1)
  
  return(result)
}

##For unweighted 
get_hist_data_raw <- function(data, var, breaks = 10) {
  hist_result <- hist(data[[var]], breaks = breaks, plot = FALSE)
  density_numeric <-hist_result$density
  breaks <- hist_result$breaks
  df <- data.frame(breaks = breaks[-length(breaks)], density = density_numeric)
  return(df)
}



