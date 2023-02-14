###Analyst, Data Science - Technical Exercise

##Author: Tom Prendergast

##Date: 14/02/2023

library(tidyverse)
library(readxl)
library(stringr)


###Read in data with clean headers

g_headers <- read_excel("008272 - Analyst Data Science - Technical Assessment - Data.xlsx", sheet = 1, n_max=2, col_names = FALSE) %>%
  t() %>%
  as_tibble() %>%
  unite("names", 1:2, sep = "_")
  
g_headers_vec <- c("Firm", g_headers$names)  

general_data <- read_excel("008272 - Analyst Data Science - Technical Assessment - Data.xlsx", sheet = 1, skip=2, col_names = g_headers_vec)

  
u_headers <- read_excel("008272 - Analyst Data Science - Technical Assessment - Data.xlsx", sheet=2, n_max=2, col_names = FALSE) %>%
t() %>%
  as_tibble() %>%
  unite("names", 1:2, sep = "_")

u_headers_vec <- c("Firm", u_headers$names) 

underwriting_data <- read_excel("008272 - Analyst Data Science - Technical Assessment - Data.xlsx", sheet=2, skip=2, col_names = u_headers_vec)


###Clean data - remove rows where zero is reported for every variable. This is done by filtering for only rows which sum to greater than zero.

general_data_clean <- general_data[rowSums(general_data[,2:length(general_data)])>0,]

underwriting_data_clean <- underwriting_data[rowSums(underwriting_data[,2:length(underwriting_data)])>0,]


###Convert datasets to long format for easier analysis
  
general_data_long <- general_data_clean %>%
  gather(key = "Measure", value = "Value", 2:length(general_data)) %>%
  separate(Measure, c("Measure", "Year"), sep = "_") 
  
general_data_long$Year <- gsub('.{2}$', '', general_data_long$Year)

underwriting_data_long <- underwriting_data_clean %>%
  gather(key = "Measure", value = "Value", 2:length(underwriting_data)) %>%
  separate(Measure, c("Measure", "Year"), sep = "_") 

underwriting_data_long$Year <- gsub('.{2}$', '', underwriting_data_long$Year)



####Identify and filter out likely reporting errors for variables of importance

boxplot_function_single <- function(data, measure, year){
  
  df <- as.data.frame(c(data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Year == year & Value != 0) 
  
  p99 <- quantile(df$Value, 0.99)
  p01 <- quantile(df$Value, 0.01) 
  
  df <- df %>%
    mutate(outlier_firms = case_when((Value > p99)|(Value < p01) ~ Firm,
                                     TRUE ~ ""))
  
  ggplot(df, aes(x=Year, y=Value)) +
    geom_boxplot() +
    geom_text(aes(label = outlier_firms), vjust=-0.3) +
    coord_flip() +
    theme_minimal() 
}

boxplot_function_timeseries <- function(data, measure){
  
  df <- as.data.frame(c(Firm = data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Value != 0) 
   
  p99 <- quantile(df$Value, 0.99)
  p01 <- quantile(df$Value, 0.01) 
  
  df <- df %>%
    mutate(outlier_firms = case_when((Value > p99)|(Value < p01) ~ Firm,
                                     TRUE ~ ""))
  
  ggplot(df, aes(x=Year, y=Value)) +
    geom_boxplot() +
    geom_text(aes(label = outlier_firms), hjust=-0.3) +
    theme_minimal()
}

boxplot_function_timeseries(data = general_data_long, measure = "NWP (£m)")

boxplot_function_timeseries(data = general_data_long, measure = "GWP (£m)")

boxplot_function_timeseries(data = general_data_long, measure = "SCR coverage ratio")

boxplot_function_timeseries(data = general_data_long, measure = "Total assets (£m)")

boxplot_function_timeseries(data = underwriting_data_long, measure = "Gross claims incurred (£m)")

boxplot_function_timeseries(data = underwriting_data_long, measure = "Net combined ratio")


general_data_long <- general_data_long %>%
  filter(!((Firm == "Firm 1" & Measure == "NWP (£m)" & Year == 2016) | 
           (Firm == "Firm 26" & Measure == "NWP (£m)" & Year == 2016) |
           (Firm == "Firm 1" & Measure == "SCR coverage ratio" & Year == 2017) |
           (Firm == "Firm 216" & Measure == "SCR coverage ratio" & Year == 2017) |
           (Firm == "Firm 131" & Measure == "SCR coverage ratio" & Year == 2017) |
             (Firm == "Firm 320" & Measure == "SCR coverage ratio" & Year == 2016) |
             (Firm == "Firm 66" & Measure == "SCR coverage ratio" & Year == 2018) |
             (Firm == "Firm 127" & Measure == "SCR coverage ratio") | 
             (Measure == "GWP (£m)" & Value < 0) |
             (Measure == "SCR coverage ratio" & Value < 0)))

underwriting_data_long <- underwriting_data_long %>%
  filter(!((Firm == "Firm 188" & Measure == "Net combined ratio" & Year == 2019) |
             (Firm == "Firm 166" & Measure == "Net combined ratio" & Year == 2020) |
             (Firm == "Firm 228" & Measure == "Net combined ratio" & Year == 2020) |
             (Firm == "Firm 284" & Measure == "Net combined ratio" & Year == 2020) |
           (Measure == "Net combined ratio" & Value < 0)))


#Create reinsurance ratio variable (NWP/GWP)

NWP <- general_data_long %>%
  filter(Measure == "NWP (£m)" & Value != 0) %>%
  mutate(MatchValue = paste0(Firm, "_", Year)) %>%
  rename(NWP = Value)

GWP <- general_data_long %>%
  filter(Measure == "GWP (£m)" & Value != 0) %>%
  mutate(MatchValue = paste0(Firm, "_", Year)) %>%
  rename(GWP = Value)

reinsurance_data <- inner_join(NWP, GWP, by="MatchValue") %>%
  mutate(Value = NWP/GWP) %>%
  mutate(Measure = "reinsurance_ratio") %>%
  select(Firm.x, Year.x, Measure, Value) %>%
  rename(Firm = Firm.x, Year = Year.x)

general_data_long <- rbind(general_data_long, reinsurance_data)



#######################################################################################################
############ ANALYSIS PART 1: CREATE ANALYSIS FUNCTIONS AND IDENTIFY FIRMS OF INTEREST #################
#######################################################################################################


#### 1: CREATE ANALYTICAL FUNCTIONS
##Create functions for identifying outliers at various levels

ninetyfivefunction_high <- function(data, measure, year){
  
  df <- as.data.frame(c(data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Year == year & Value != 0)
  
  p95 <- quantile(df$Value, 0.95)
  
  df[order(-df$Value),] %>%
  filter(Value > p95) %>%
  select(Firm, Value) 

} #Shows top 5% of firms in a given measure in a given period

ninetyfivefunction_low <- function(data, measure, year){
  
  df <- as.data.frame(c(data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Year == year & Value != 0)
  
  p05 <- quantile(df$Value, 0.05)
  
  df[order(df$Value),] %>%
    filter(Value < p05) %>%
    select(Firm, Value) 
  
} #Shows bottom 5% of firms in a given measure in a given period

ninety_function_high <- function(data, measure, year){
  
  df <- as.data.frame(c(data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Year == year & Value != 0)
  
  p90 <- quantile(df$Value, 0.90)
  
  df[order(-df$Value),] %>%
    filter(Value > p90) %>%
    select(Firm, Value) 
  
} #Shows top 10% of firms in a given measure in a given period

ninety_function_low <- function(data, measure, year){
  
  df <- as.data.frame(c(data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Year == year & Value != 0)
  
  p10 <- quantile(df$Value, 0.1)
  
  df[order(df$Value),] %>%
    filter(Value < p10) %>%
    select(Firm, Value) 
  
} #Shows bottom 10% of firms in a given measure in a given period

ninety_function_alloutliers <- function(data, measure, year){
  
  df <- as.data.frame(c(data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Year == year & Value != 0)
  
  p90 <- quantile(df$Value, 0.90)
  p10 <- quantile(df$Value, 0.1)
  
  df[order(df$Value),] %>%
    filter(Value < p10 | Value > p90) %>%
    select(Firm, Value) 
  
} #Shows top and bottom 10% of firms in a given measure in a given period

ninetyfive_function_alloutliers <- function(data, measure, year){
  
  df <- as.data.frame(c(data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Year == year & Value != 0)
  
  p95 <- quantile(df$Value, 0.95)
  p05 <- quantile(df$Value, 0.05)
  
  df[order(df$Value),] %>%
    filter(Value < p05 | Value > p95) %>%
    select(Firm, Value) 
  
} #Shows top and bottom 5% of firms in a given measure in a given period


#Create function to examine growth rates

growthrate_function <- function(data, measure, year1, year2){
  
  df_y1 <- as.data.frame(c(Firm = data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year_1 = 3, Value_yr1 = 4) %>%
    filter(Measure == measure & Value_yr1 != 0 & Year_1 == year1) 
  
  df_y2 <- as.data.frame(c(Firm = data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year_2 = 3, Value_yr2 = 4) %>%
    filter(Measure == measure & Value_yr2 != 0 & Year_2 == year2)
  
  df_merged <- inner_join(df_y1, df_y2, by = "Firm") %>%
    mutate(growth = Value_yr2/Value_yr1-1)
  
  
  
} #Shows growth rates for a given measure in a given two year period



####  2: IDENTIFY FIRMS OF CONCERN 

####Isolate problematic firms for key variables

#Gross written premiums - Identify largest firms and substantial changes in turnover

ninetyfivefunction_high(data = general_data_long, measure = "GWP (£m)", year = 2020)

ninetyfivefunction_low(data = general_data_long, measure = "SCR coverage ratio", year = 2020)

high_GWP <- ninety_function_high(data = general_data_long, measure = "GWP (£m)", year = 2020)

high_GWP <- as.vector(high_GWP$Firm)

GWP_growthrates <- growthrate_function(data = general_data_long, measure = "GWP (£m)", year1 = 2019, year2 = 2020) %>%
  select(Firm, Measure.x, Year_2, growth) %>%
  rename(Measure = Measure.x, Year = Year_2, Value = growth)
  
GWP_outlier_growthrates <- ninety_function_alloutliers(data = GWP_growthrates, measure = "GWP (£m)", year = 2020)

GWP_outlier_growthrates <- as.vector(GWP_outlier_growthrates$Firm)



#SCR coverage ratio - Identify firms with insufficient SCR coverage and downward change in coverage
low_SCRcoverage <- ninety_function_low(data = general_data_long, measure = "SCR coverage ratio", year = 2020)

low_SCRcoverage <- as.vector(low_SCRcoverage$Firm)

SCR_growthrates <- growthrate_function(data = general_data_long, measure = "SCR coverage ratio", year1 = 2019, year2 = 2020) %>%
  select(Firm, Measure.x, Year_2, growth) %>%
  rename(Measure = Measure.x, Year = Year_2, Value = growth)

SCR_low_growthrates <- ninety_function_low(data = SCR_growthrates, measure = "SCR coverage ratio", year = 2020)

SCR_low_growthrates <- as.vector(SCR_low_growthrates$Firm)

  
#Net combined ratio - Identify unprofitable firms and upward change in NCR/unprofitability

NCR_over100 <- underwriting_data_long %>%
  filter(Measure == "Net combined ratio" & Year == 2020 & Value > 1)

NCR_over100 <- as.vector(NCR_over100$Firm)

NCR_growthrates <- growthrate_function(data = underwriting_data_long, measure = "Net combined ratio", year1 = 2019, year2 = 2020) %>%
  select(Firm, Measure.x, Year_2, growth) %>%
  rename(Measure = Measure.x, Year = Year_2, Value = growth)

NCR_high_growthrates <- ninety_function_high(data = NCR_growthrates, measure = "Net combined ratio", year = 2020)

NCR_high_growthrates <- as.vector(NCR_high_growthrates$Firm)


#Identify firms with high levels of reinsurance (NWP/GWP - lower ratio = higher rate of reinsurance), and spot upwards or downwards trends in reinsurance

low_reinsurance_ratio <- ninety_function_low(data = general_data_long, measure = "reinsurance_ratio", year = 2020)

low_reinsurance_ratio <- as.vector(low_reinsurance_ratio$Firm)

rr_growthrates <- growthrate_function(data = general_data_long, measure = "reinsurance_ratio", year1 = 2019, year2 = 2020) %>%
  select(Firm, Measure.x, Year_2, growth) %>%
  rename(Measure = Measure.x, Year = Year_2, Value = growth)

rr_outlier_growthrates <- ninety_function_alloutliers(data = rr_growthrates, measure = "reinsurance_ratio", year = 2020)

rr_outlier_growthrates <- as.vector(rr_outlier_growthrates$Firm)


#Identify firms with high gross claims incurred or strong increases in GCI

high_GCI <- ninety_function_high(data = underwriting_data_long, measure = "Gross claims incurred (£m)", year = 2020)

high_GCI <- as.vector(high_GCI$Firm)

GCI_growthrates <- growthrate_function(data = underwriting_data_long, measure = "Gross claims incurred (£m)", year1 = 2019, year2 = 2020) %>%
  select(Firm, Measure.x, Year_2, growth) %>%
  rename(Measure = Measure.x, Year = Year_2, Value = growth)

GCI_high_growthrates <- ninety_function_high(data = GCI_growthrates, measure = "Gross claims incurred (£m)", year = 2020)

GCI_high_growthrates <- as.vector(GCI_high_growthrates$Firm)


#Identify firms with a negative change in equity

equity_negative_growth <- growthrate_function(data = general_data_long, measure = "Excess of assets over liabilities (£m) [= equity]", year1 = 2019, year2 = 2020) %>%
  filter(growth < 0)

equity_negative_growth <- as.vector(equity_negative_growth$Firm)


####Combine: assemble table of above metrics and identify most problematic firms

compare_metrics <- as.data.frame(general_data$Firm) %>% 
  rename(Firm =1 ) %>%
                    mutate(highGWP = case_when(Firm %in% high_GWP ~ 1,
                                               TRUE ~ 0)) %>%
  mutate(GWP_outlier_growthrates = case_when(Firm %in% GWP_outlier_growthrates ~ 1,
                             TRUE ~ 0)) %>%
  mutate(low_SCRcoverage = case_when(Firm %in% low_SCRcoverage ~ 1,
                                             TRUE ~ 0)) %>%
  mutate(SCR_low_growthrates = case_when(Firm %in% SCR_low_growthrates ~ 1,
                                     TRUE ~ 0)) %>%
  mutate(NCR_over100 = case_when(Firm %in% NCR_over100 ~ 1,
                                         TRUE ~ 0)) %>%
  mutate(NCR_high_growthrates = case_when(Firm %in% NCR_high_growthrates ~ 1,
                                 TRUE ~ 0)) %>%
  mutate(low_reinsurance_ratio = case_when(Firm %in% low_reinsurance_ratio ~ 1,
                                         TRUE ~ 0)) %>%
  mutate(rr_outlier_growthrates = case_when(Firm %in% rr_outlier_growthrates ~ 1,
                                           TRUE ~ 0)) %>%
  mutate(high_GCI = case_when(Firm %in% high_GCI ~ 1,
                                            TRUE ~ 0)) %>%
  mutate(GCI_high_growthrates = case_when(Firm %in% GCI_high_growthrates ~ 1,
                              TRUE ~ 0)) %>%
  mutate(equity_negative_growth = case_when(Firm %in% equity_negative_growth ~ 1,
                                          TRUE ~ 0)) 

metric_totals <- rowSums(compare_metrics[,2:length(compare_metrics)]) 

compare_metrics <- cbind(compare_metrics, metric_totals) %>%
  filter(metric_totals > 0)


################################################################################
############ ANALYSIS PART 2: FUTHER EXAMINE PROBLEMATIC FIRMS #################
################################################################################

##Create visualisation functions for ease of analysis

timeseries_function <- function(data, measure, ylab){
  
  df <- as.data.frame(c(Firm = data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure & Value != 0) 
  
  
  ggplot(df, aes(x=Year, y=Value, group=1)) +
    geom_point(color = "blue4") +
    geom_line(color = "blue4")+
    theme_minimal() +
    ylab(ylab)
}

comparison_function <- function(data, measure_x, measure_y, xlab, ylab){
  
  df1 <- as.data.frame(c(Firm = data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure_x & Value != 0) 
  
  df2 <- as.data.frame(c(Firm = data["Firm"], data["Measure"], data["Year"], data["Value"])) %>%
    rename(Firm = 1, Measure = 2, Year = 3, Value = 4) %>%
    filter(Measure == measure_y & Value != 0) %>%
    select(Year, Value)
  
  df_joined <- inner_join(df1, df2, by = "Year")
  
  ggplot(df_joined, aes(x=Value.x, y=Value.y)) +
    geom_point() +
    geom_text(aes(label = Year), hjust=-0.3) +
    theme_minimal() +
    xlab(xlab) +
    ylab(ylab)
}


#Firm 26: large size, strong negative growth in SCR coverage, unprofitable, negative equity growth

firm_26 <- general_data_long %>%
  filter(Firm == "Firm 26")

firm_26_underwriting <- underwriting_data_long %>%
  filter(Firm == "Firm 26")

firm_26 <- rbind(firm_26, firm_26_underwriting)


timeseries_function(data = firm_26, measure = "SCR coverage ratio", ylab = "SCR coverage ratio")

timeseries_function(data = firm_26, measure = "Excess of assets over liabilities (£m) [= equity]", ylab = "Equity (£m)")



#Firm 210: Large size, low SCR coverage, strong negative growth in SCR coverage, rapid increase in unprofitability

firm_210 <- general_data_long %>%
  filter(Firm == "Firm 210")

firm_210_underwriting <- underwriting_data_long %>%
  filter(Firm == "Firm 210")

firm_210 <- rbind(firm_210, firm_210_underwriting)


timeseries_function(data = firm_210, measure = "SCR coverage ratio", ylab = "SCR coverage ratio")

timeseries_function(data = firm_210, measure = "Net combined ratio", ylab = "Net combined ratio")



#Firm 234: large size, low SCR coverage, strong decline in SCR coverage, high gross claims incurred, negative equity growth

firm_234 <- general_data_long %>%
  filter(Firm == "Firm 234")

firm_234_underwriting <- underwriting_data_long %>%
  filter(Firm == "Firm 234")

firm_234 <- rbind(firm_234, firm_234_underwriting)

timeseries_function(data = firm_234, measure = "SCR coverage ratio", ylab = "SCR coverage ratio")

timeseries_function(data = firm_234, measure = "Gross claims incurred (£m)", ylab = "Gross claims incurred (£m)")


#Firm 7: large size, low SCR coverage, strong decline in SCR coverage 

firm_7 <- general_data_long %>%
  filter(Firm == "Firm 7")

firm_7_underwriting <- underwriting_data_long %>%
  filter(Firm == "Firm 7")

firm_7 <- rbind(firm_7, firm_7_underwriting)

timeseries_function(data = firm_7, measure = "SCR coverage ratio", ylab = "SCR coverage ratio")


#Firm 34: unprofitable, rapid change in reinsurance, negative equity growth

firm_34 <- general_data_long %>%
  filter(Firm == "Firm 34")

firm_34_underwriting <- underwriting_data_long %>%
  filter(Firm == "Firm 34")

firm_34 <- rbind(firm_34, firm_34_underwriting)

timeseries_function(data = firm_34, measure = "SCR coverage ratio", ylab = "SCR coverage ratio")

timeseries_function(data = firm_34, measure = "reinsurance_ratio", ylab = "Reinsurance ratio")

timeseries_function(data = firm_34, measure = "Excess of assets over liabilities (£m) [= equity]", ylab = "Equity (£m)")


#Firm 247: variable SCR coverage, strong growth in reinsurance, negative equity growth

firm_247 <- general_data_long %>%
  filter(Firm == "Firm 247")

firm_247_underwriting <- underwriting_data_long %>%
  filter(Firm == "Firm 247")

firm_247 <- rbind(firm_247, firm_247_underwriting)

timeseries_function(data = firm_247, measure = "SCR coverage ratio", ylab = "SCR coverage ratio")

timeseries_function(data = firm_247, measure = "reinsurance_ratio", ylab = "Reinsurance ratio")


#Firm 151: oddly strong GWP growth, negative equity growth

firm_151 <- general_data_long %>%
  filter(Firm == "Firm 151")

firm_151_underwriting <- underwriting_data_long %>%
  filter(Firm == "Firm 151")

firm_151 <- rbind(firm_151, firm_151_underwriting)

timeseries_function(data = firm_151, measure = "GWP (£m)", ylab = "GWP (£m)")



#####Combined plot: SCR ratio coverage (Firms 26, 210, 234, 7)

SCR_chart_firms <- general_data_long %>%
  filter(Firm %in% c("Firm 26", "Firm 210", "Firm 234", "Firm 7") & Measure == "SCR coverage ratio") 
  

ggplot(SCR_chart_firms, aes(x=Year, y=Value, group=Firm, color = Firm)) +
  geom_point() +
  geom_line()+
  theme_minimal() +
  ylab("SCR coverage ratio")


#####Combined plot: Reinsurance

rr_chart_firms <- general_data_long %>%
  filter(Firm %in% c("Firm 34", "Firm 247") & Measure == "reinsurance_ratio") 


ggplot(rr_chart_firms, aes(x=Year, y=Value, group=Firm, color = Firm)) +
  geom_point() +
  geom_line()+
  theme_minimal() +
  ylab("Reinsurance ratio (NWP/GWP)")


#####Combined plot: NCR

NCR_chart_firms <- underwriting_data_long %>%
  filter(Firm %in% c("Firm 26", "Firm 210") & Measure == "Net combined ratio") 


ggplot(NCR_chart_firms, aes(x=Year, y=Value, group=Firm, color = Firm)) +
  geom_point() +
  geom_line()+
  theme_minimal() +
  ylab("Net combined ratio")

SCR_2020 <- general_data_long %>%
  filter(Measure == "SCR coverage ratio" & Year == 2020)


