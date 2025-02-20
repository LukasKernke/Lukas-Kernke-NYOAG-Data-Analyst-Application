#### Goal & Notes #### 
# This was for a freelance position in which my assigned task was to:
# find organizations with 3 or more years of consecutive deficit in given datasets
# Datasets are of arts organizations operating in NYC and filling 990 forms between 2010-2019
# Datasets were cleaned in a previous script
# The datasets below are subsets of the cleaned datasets now including only organizations with deficits 
# FUNDBAL variable refers to funding balance at end of year, TOTREV refers to total revenue at end of year
# OoC refers to Organizations of Color and can be Yes (Y), No (N), or Missing Data (MD) rather than NA at request of employer

#### Libraries ####
library(tidyverse)
library(ggrepel)
library(geomtextpath)

#### Importing Data ####
fundbal_deficit_orgs_2010 <- read_csv("data/fundbal_deficit_orgs_2010.csv")
fundbal_deficit_orgs_2011 <- read_csv("data/fundbal_deficit_orgs_2011.csv")
fundbal_deficit_orgs_2012 <- read_csv("data/fundbal_deficit_orgs_2012.csv")
fundbal_deficit_orgs_2013 <- read_csv("data/fundbal_deficit_orgs_2013.csv")
fundbal_deficit_orgs_2014 <- read_csv("data/fundbal_deficit_orgs_2014.csv")
fundbal_deficit_orgs_2015 <- read_csv("data/fundbal_deficit_orgs_2015.csv")
fundbal_deficit_orgs_2016 <- read_csv("data/fundbal_deficit_orgs_2016.csv")
fundbal_deficit_orgs_2017 <- read_csv("data/fundbal_deficit_orgs_2017.csv")
fundbal_deficit_orgs_2018 <- read_csv("data/fundbal_deficit_orgs_2018.csv")
fundbal_deficit_orgs_2019 <- read_csv("data/fundbal_deficit_orgs_2019.csv")

#### Joining Data #### 
fundbal_deficit_orgs_joined <- bind_rows(fundbal_deficit_orgs_2010, 
                                         fundbal_deficit_orgs_2011,
                                         fundbal_deficit_orgs_2012, 
                                         fundbal_deficit_orgs_2013, 
                                         fundbal_deficit_orgs_2014, 
                                         fundbal_deficit_orgs_2015, 
                                         fundbal_deficit_orgs_2016, 
                                         fundbal_deficit_orgs_2017, 
                                         fundbal_deficit_orgs_2018, 
                                         fundbal_deficit_orgs_2019) %>% 
  mutate(`pct_deficit` = ((FUNDBAL/TOTREV) * 100)) 
# Percent deficit is calculated by dividing Funding Balance (FUNDBAL) by Total Revenue (TOTREV) * 100
# Percent deficit was ultimately not used, but may be useful in the future

#### counting categories ####
# counting total number of organizations with deficits each year
count_fundbal_deficit_orgs_joined <- fundbal_deficit_orgs_joined %>% 
  count(year)

# counting number of organizations of color with deficits each year
count_fundbal_deficit_ooc_orgs_joined <- fundbal_deficit_orgs_joined %>% 
  filter(`OOC?` == "Y") %>% 
  count(year) 

# counting number of non-organizations of color with deficits each year
count_fundbal_deficit_non_ooc_orgs_joined <- fundbal_deficit_orgs_joined %>% 
  filter(`OOC?` == "N") %>% 
  count(year) 

#### visualizing orgs with deficits over time #### 
# this provides a useful overview of the data over time
count_fundbal_deficit_orgs_plot <- ggplot(count_fundbal_deficit_orgs_joined) +
  geom_line(aes(year, n), color = "darkgrey") + # line plot for overall
  geom_point(aes(year, n), color = "darkgrey") + 
  geom_textline(aes(year, n, label = "All Organizations"), hjust = .13, vjust = -0.7, 
                size = 3, color = "darkgrey") +
  geom_line(data = count_fundbal_deficit_ooc_orgs_joined, aes(year, n), linetype = "longdash") + # line plot for ooc
  geom_point(data = count_fundbal_deficit_ooc_orgs_joined, aes(year, n)) + 
  geom_textline(data = count_fundbal_deficit_ooc_orgs_joined, 
                aes(year, n, label = "Organizations of Color"), hjust = .05,  vjust = -0.7, 
                size = 3, linetype = "longdash") +
  geom_line(data = count_fundbal_deficit_non_ooc_orgs_joined, aes(year, n)) + # line plot for non-ooc
  geom_point(data = count_fundbal_deficit_non_ooc_orgs_joined, aes(year, n)) + 
  geom_textline(data = count_fundbal_deficit_non_ooc_orgs_joined, 
                aes(year, n, label = "Non-Organizations of Color"), hjust = .02,  vjust = -0.7, size = 3) +
  labs(title = "Number of Organizations with Deficits",
       x = "Year", y = "Number") + 
  geom_text_repel(
    aes(year, n, label = n), data = (count_fundbal_deficit_ooc_orgs_joined %>% filter(year == max(year))),
    color = "black", nudge_x = 0.35, size = 2) + # 2019 label for OoC
  geom_text_repel(
    aes(year, n, label = n), data = (count_fundbal_deficit_ooc_orgs_joined %>% filter(year == min(year))),
    color = "black", size = 2, vjust = 1) + # 2010 label for OoC
  geom_text_repel(
    aes(year, n, label = n), data = (count_fundbal_deficit_orgs_joined %>% filter(year == max(year))),
    color = "darkgrey", nudge_x = 0.35, size = 2) + # 2019 label for overall
  geom_text_repel(
    aes(year, n, label = n), data = (count_fundbal_deficit_orgs_joined %>% filter(year == min(year))),
    color = "darkgrey", size = 2, vjust = 1) + # 2010 label for overall
  geom_text_repel(
    aes(year, n, label = n), data = (count_fundbal_deficit_non_ooc_orgs_joined %>% filter(year == max(year))),
    color = "black", nudge_x = 0.35, size = 2) + # 2019 label for non-OoC
  geom_text_repel(
    aes(year, n, label = n), data = (count_fundbal_deficit_non_ooc_orgs_joined %>% filter(year == min(year))),
    color = "black", size = 2, vjust = 1) + # 2010 label for non-OoC
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017,
                                2018, 2019)) + 
  theme_bw()


ggsave("count_fundbal_deficit_orgs_plot.png", 
       plot = count_fundbal_deficit_orgs_plot, 
       units = "in", 
       height = 5, width = 7)


#### Finding Consecutive deficit #### 
# Looking for orgs that have 3 consecutive years of deficit 

# ordering by name, then year, and filtering to orgs. that appear 3 or more times
fundbal_deficit_duplicates <- fundbal_deficit_orgs_joined %>% 
  arrange(fundbal_deficit_orgs_joined$NAME, 
          fundbal_deficit_orgs_joined$year, .by_group = TRUE) %>% 
  group_by(NAME)%>% # ordering and grouping by name
  filter(n()>2) %>% # filtering to entries that appear more than two times
  select("year", "NAME", "pct_deficit", "Racial Category", "Discipline", 
         "Diversity/Social Justice Emphasis?", "OOC?", "Other Identity Group", 
         "Diversifying", "Cultural Heritage", "closed?", "# of foundation grants", 
         "total foundation grant income", "FUNDBAL", "NETA_BOY",
         "NETINC", "EXPS", "TOTREV", "CONT", "ZIP", "# Foundation Grants", 
         "$Amt Foundation Grants") # using select to change col order to make easier to eyeball data


# filtering to orgs with deficit over consecutive years
fundbal_consecutive_deficit <- fundbal_deficit_duplicates %>% 
  group_by(seq_id = cumsum(fundbal_deficit_duplicates$year 
                           != lag(fundbal_deficit_duplicates$year) + 1 
                           | is.na(fundbal_deficit_duplicates$year 
                                   != lag(fundbal_deficit_duplicates$year) + 1)), 
           NAME,
           seq_id) %>% # organizing by sequential years of deficit
  filter(n()>2) # filter to orgs that appear MORE than twice

write_csv(fundbal_consecutive_deficit, "fundbal_consecutive_deficit.csv")


# make each org one observation to avoid miscounting
fundbal_orgs_consec_def <- fundbal_consecutive_deficit %>% 
  group_by(NAME, `OOC?`) %>% # OoC refers to Organizations of Color
  nest()

write_csv(fundbal_orgs_consec_def, "fundbal_orgs_consec_def.csv")


# grouping and counting 
fundbal_consec_def_ooc_count <- fundbal_orgs_consec_def %>%
  select(NAME, `OOC?`) %>% 
  ungroup() %>%
  count(`OOC?`) %>% 
  mutate(`pct` = n / 336 * 100) 
# 336 is the total number of organizations with 3 or more years of consecutive deficit
# therefore n/336 * 100 calculated the percent of all organizations 

write_csv(fundbal_consec_def_ooc_count, "fundbal_consec_def_ooc_count.csv")

#### plotting count ####
fundbal_consec_def_ooc_count_col_plot <- ggplot() +
  geom_col(data = fundbal_consec_def_ooc_count,
           aes(x = `OOC?`,
               y = n)) +
  labs(x = "Organization of Color?",
       y = "Count",
       title = "Organizations with 3+ Years of Consecutive Deficits") +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5)) +
  theme_bw()

ggsave("fundbal_consec_def_ooc_count_col_plot.png", 
       plot = fundbal_consec_def_ooc_count_col_plot, 
       units = "in", 
       height = 5, width = 7)

#### plotting percent #### 
# plotting percent as pie chart
fundbal_consec_def_ooc_pct_pie_plot <- ggplot(data = fundbal_consec_def_ooc_count,
                                              aes(x = "", y = pct, fill = `OOC?`)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(title = "Percent Organizations with 3+ Years of Consecutive Deficits") +
  theme_bw()

ggsave("fundbal_consec_def_ooc_pct_pie_plot.png", 
       plot = fundbal_consec_def_ooc_pct_pie_plot, 
       units = "in", 
       height = 5, width = 7)

