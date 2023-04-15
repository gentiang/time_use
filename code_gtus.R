# Loading libraries
library(haven)
library(tidyverse)
library(janitor)
library(readxl)
library(ggstream)
library(glue)
library(scales)
library(dutchmasters)
library(showtext)
library(hms)
################################################################################
# Setting base assets
theme_set(theme_minimal())
clrs <- c("#9d3d2d", "#99a8a5", "#8f4c2d", "#6a6a53", "#F0984A", "#272220", "#727c73", "#c9aa82", "#3a2d22", "#8f5144", "#FCD6A5")
hm_clrs <- c("#1C4C70", "#9d3d2d", "#7AA9A3", "#FCD6A5", "#F0984A", "#D95F3B", "#99a8a5", "#8f4c2d", "#6a6a53", "#eadac5")
time_seg_fct <- c("4:00",	"4:15",	"4:30",	"4:45",	"5:00",	"5:15",	"5:30",	"5:45",	
                   "6:00",	"6:15",	"6:30",	"6:45",	"7:00",	"7:15",	"7:30",	"7:45",	
                   "8:00",	"8:15",	"8:30",	"8:45",	"9:00",	"9:15",	"9:30",	"9:45",	
                   "10:00",	"10:15",	"10:30",	"10:45",	"11:00",	"11:15",	"11:30",	
                   "11:45",	"12:00",	"12:15",	"12:30",	"12:45",	"13:00",	"13:15",	
                   "13:30",	"13:45",	"14:00",	"14:15",	"14:30",	"14:45",	"15:00",	
                   "15:15",	"15:30",	"15:45",	"16:00",	"16:15",	"16:30",	"16:45",	
                   "17:00",	"17:15",	"17:30",	"17:45",	"18:00",	"18:15",	"18:30",	
                   "18:45",	"19:00",	"19:15",	"19:30",	"19:45",	"20:00",	"20:15",	
                   "20:30",	"20:45",	"21:00",	"21:15",	"21:30",	"21:45",	"22:00",	
                   "22:15",	"22:30",	"22:45",	"23:00",	"23:15",	"23:30",	"23:45",	
                   "00:00 (nd)",	"00:15 (nd)",	"00:30 (nd)",	"00:45 (nd)",	"1:00 (nd)",	
                   "1:15 (nd)",	"1:30 (nd)",	"1:45 (nd)",	"2:00 (nd)",	"2:15 (nd)",	"2:30 (nd)",	
                   "2:45 (nd)",	"3:00 (nd)",	"3:15 (nd)",	"3:30 (nd)",	"3:45 (nd)")

time_hrs_fct <- c("4:00",	"5:00",	"6:00",	"7:00",	"8:00",	"9:00",	"10:00",	"11:00",	
                  "12:00",	"13:00",	"14:00",	"15:00",	"16:00",	"17:00",	"18:00",	
                  "19:00",	"20:00",	"21:00",	"22:00",	"23:00",	"00:00 (nd)",	
                  "1:00 (nd)",	"2:00 (nd)",	"3:00 (nd)")

## Font
font_add_google("PT Sans", "pt")
showtext_auto()
showtext_opts(dpi = 96)

ft <- "pt"
################################################################################
# Reading data
tus <- read_dta("gtus.dta")
################################################################################
# Preliminary processing
tus <- tus |> 
  select(respondent_id_anon, ps_sex, pr_seg1:pr_seg96)

glimpse(tus)

tus <- tus |> 
  filter(!is.na(pr_seg1),
         !is.na(ps_sex))

df <- tus |> 
  pivot_longer(cols = c(pr_seg1:pr_seg96), names_to = "pr_segment", 
               values_to = "activity_code") |> 
  mutate(activity_code = as.double(activity_code))

activity <- read_xlsx("activity_table.xlsx", sheet = 1)
time <- read_xlsx("activity_table.xlsx", sheet = 2)

df2 <- df |> 
  left_join(activity, by = "activity_code") |> 
  left_join(time, by = "pr_segment") |> 
  mutate(activity = str_to_sentence(activity),
         category = str_to_title(category),
         time_segment_value = 0.25,
         category = fct_relevel(category, c("Personal Care", "Employment",
                                            "Studying", "Household And Family Care",
                                            "Voluntary Work And Meetings",
                                            "Social Life And Entertainment",
                                            "Sports And Outdoor Activities",
                                            "Hobbies And Computing", "Mass Media",
                                            "Travel", "Other"))) |> 
  relocate(c(time_segment, time_segment_s1, time_segment_hour, time_segment_value), .after = pr_segment ) 
  
################################################################################
# EDA

df2 |> 
  tabyl(category) |> 
  as_tibble()

df2 |> 
  tabyl(activity) |> 
  as_tibble()

missing <- df2 |> 
  filter(is.na(category))

df2 |> 
  tabyl(ps_sex)
################################################################################
# Graphs

## Bar graph of categories by hours spent
df2 |>
  filter(!is.na(category)) |> 
  group_by(category) |> 
  summarise(time_spent = sum(time_segment_value)) |>
  mutate(percent = prop.table(time_spent)) |> 
  ggplot(aes(x = fct_rev(fct_reorder(category, percent)), y = percent, fill = category)) +
  geom_col() +
  geom_text(aes(label = glue("{category} \n {percent(percent, accuracy = 0.1)}")), 
            hjust = 0, nudge_y = 0.005, size = 8, fontface = "bold", family = ft, color = clrs) +
  scale_y_continuous(limits = c(0,0.55)) +
  coord_flip() +
  scale_fill_manual(values = clrs) +
  labs(
    title = "Time Spent on Activity",
    subtitle = "% of total hours across sample population within a 24-hour time period"
  ) +
  theme(
        legend.position = "none",
        text = element_text(family = ft),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(1,1,1,1, unit="cm"),
        #plot.title = element_text(size = 25),
        #plot.subtitle = element_text(size = 15)
        plot.title = element_blank(),
        plot.subtitle = element_blank()
        )

ggsave("activity.pdf", device = "pdf", width = 1365, height = 1017, unit = "px", scale = 3)

## Heatmap of persons spending time by time of day and activity

# hm <- df2 |>
#   mutate(person_per_activity = 1,
#          time_segment_s1 = fct_relevel(time_segment_s1, time_seg_fct),
#          time_segment_hour = fct_relevel(time_segment_hour, time_hrs_fct)) |> #at any time of the day for any category there's one person doing something (and that's what u use to count up the number of people per time and activity)
#   group_by(category, time_segment_s1, time_segment_hour) |>
#   summarise(people = sum(person_per_activity)) |>
#   ungroup() |> 
#   mutate(group_total = sum(people), .by = time_segment_s1) |> 
#   mutate(perc_people = people/group_total)
# 
# hm |>
#   filter(!is.na(category)) |> 
#   ggplot(aes(x = time_segment_hour, y = fct_rev(category), fill = perc_people)) +
#   geom_tile(color = "white",
#             lwd = 1.5,
#             linetype = 1) +
#   coord_fixed() +
#   scale_fill_stepsn("", colours = c("#1C4C70", "#F0984A", "#9d3d2d"), breaks = c(seq(0,1,0.1)), limits = c(0,1), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
#   guides(fill = guide_colourbar(barwidth = 0.7,
#                                 barheight = 38.5)) +
#   #geom_vline(xintercept = 5.5, size = 1.5, linetype = 5) +
#   labs(
#     title = "How Many People are Engaged in an Activity at a Certain Time of Day?",
#     subtitle = "% of total individuals (8578 interviewed) at time of day",
#     caption = "Note: \"nd\" stands for next day"
#   ) +
#   theme(
#     text = element_text(family = ft),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = margin(1,1,1,1, unit="cm"),
#     plot.title = element_text(size = 25),
#     plot.title.position = "plot",
#     plot.subtitle = element_text(size = 15),
#     axis.text = element_text(size = 15),
#     axis.text.x = element_text(angle = -45, vjust = -0.5),
#     legend.text = element_text(size = 12),
#     plot.caption = element_text(size = 13, hjust = 0)
#   )

hm2 <- df2 |>
  mutate(person_per_activity = 1,
         time_segment_s1 = fct_relevel(time_segment_s1, time_seg_fct),
         time_segment_hour = fct_relevel(time_segment_hour, time_hrs_fct)) |> #at any time of the day for any category there's one person doing something (and that's what u use to count up the number of people per time and activity)
  group_by(category, time_segment_s1, time_segment_hour) |>
  summarise(people = sum(person_per_activity)) |>
  ungroup() |> 
  mutate(group_total = sum(people), .by = time_segment_s1) |> 
  mutate(perc_people = people/group_total) |> 
  group_by(category, time_segment_hour) |> 
  summarise(perc_people = mean(perc_people))

hm2 |>
  filter(!is.na(category)) |> 
  ggplot(aes(x = time_segment_hour, y = fct_rev(category), fill = perc_people)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed() +
  scale_fill_stepsn("", colours = c("#1C4C70", "#F0984A", "#9d3d2d"), breaks = c(seq(0,1,0.1)), limits = c(0,1), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  guides(fill = guide_colourbar(barwidth = 0.7,
                                barheight = 22)) +
  #geom_vline(xintercept = 5.5, size = 1.5, linetype = 5) +
  labs(
    title = "How Many People are Engaged in an Activity at a Certain Time of Day?",
    subtitle = "% of total individuals (8578 interviewed) at time of day",
    caption = "Note: \"nd\" stands for next day"
  ) +
  theme(
    text = element_text(family = ft),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(1,1,1,1, unit="cm"),
    #plot.title = element_text(size = 25),
    plot.title.position = "plot",
    #plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = -45, vjust = -0.5),
    legend.text = element_text(size = 12),
    #plot.caption = element_text(size = 13, hjust = 0)
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank()
  )

ggsave("heatmap.pdf", device = "pdf", width = 1365, height = 800, unit = "px", scale = 3)


## Pyramid chart of gender distribution - time spent in certain activities
gender_time <- df2 |> 
  filter(!is.na(category)) |> 
  group_by(respondent_id_anon, ps_sex, category) |> 
  summarise(time_spent = sum(time_segment_value)) |> 
  ungroup() |> 
  group_by(category, ps_sex) |> 
  summarise(avg_time_spent = mean(time_spent)) |> 
  mutate(ps_sex = as.factor(ps_sex),
         time_in_hours = hms(hours = avg_time_spent),
         hours = hour(time_in_hours),
         minutes = minute(time_in_hours))

gender_time |> 
  ggplot(aes(x = fct_rev(category), fill = ps_sex)) +
  geom_bar(data = subset(gender_time, ps_sex == 1), aes(y = -avg_time_spent), stat = "identity") + 
  geom_bar(data = subset(gender_time, ps_sex == 2), aes(y = avg_time_spent), stat = "identity") +
   geom_text(data = subset(gender_time, ps_sex == 1 & category %in% c("Employment", "Household And Family Care")), aes(label = glue("{hours}h {minutes}m"), y = -avg_time_spent), 
             size = 5, fontface = "bold", family = ft, color = "white", hjust = -0.1) +
   geom_text(data = subset(gender_time, ps_sex == 2 & category %in% c("Employment", "Household And Family Care")), aes(label = glue("{hours}h {minutes}m"), y = avg_time_spent), 
             size = 5, fontface = "bold", family = ft, color = "white", hjust = 1.1) + 
  coord_flip() +
  scale_y_continuous() +
  scale_fill_manual("", values = c("#1C4C70", "#9d3d2d"), label = c("Men", "Women")) +
  labs(
    title = "Time Spent on Each Activity by Gender",
    subtitle = "average hours within a 24-hour period"
  ) +
  theme(
    text = element_text(family = ft),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(1,1,1,1, unit="cm"),
    #plot.title = element_text(size = 25),
    plot.title.position = "plot",
    #plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    #legend.position = c(0.8, 0.2)
    legend.position = "none",
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
  )

ggsave("gender.pdf", device = "pdf")

