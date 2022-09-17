library(tidyverse)
library(ggplot2)
library(scales)
library(hrbrthemes)
library(ggthemes)
library(gganimate)

install.packages('ggthemes', dependencies = TRUE)

data <- read.csv("data/GLB.Ts+dSST.csv",skip = 1, na="***") %>% 
  select(year = Year, t_diff = 'J.D') %>% 
  na.exclude()

# Plotting a basic line graph ----
data %>% 
  ggplot(aes(x=year, y=t_diff)) + 
  geom_line(color="gray", size = 0.5, show.legend = FALSE) + 
  geom_point(fill="white", aes(color="1"), shape=21, show.legend = TRUE) +
  geom_smooth(se=FALSE, aes(color="2"), size = 0.5, span = 0.25, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1880, 2023, 20), expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.5, 1.5)) + 
  scale_color_manual(name=NULL, 
                     breaks = c(1,2), 
                     values = c("gray", "red"), 
                     labels = c("Annual mean", "Lowess smoothing"), 
                     guide = guide_legend(override.aes = list(shape=15))) + ## <- cool technique to override line color to box color in the legend
  labs(x = "Year", 
       y = "Temperature change", 
       title = "Global Land-Ocean Temperature Index", 
       subtitle = "Data Source: NASA/GISS") +
  theme_light() + 
  theme(plot.title = element_text(color = "red", face = "bold"))

# Plotting a bar graph ---- 

annotation <- data %>%
  arrange(year) %>% 
  slice(1,n())  %>% # to get the first and last row of the data frame
  mutate(t_diff = 0, 
         x = year + c(-5,5))

data %>% 
  ggplot(aes(x=year,y=t_diff, fill = t_diff)) + 
  geom_col(show.legend = FALSE) + 
  geom_text(data = annotation, aes(x=x,label=year), color = "white") +
  #scale_fill_gradient2(low="darkblue", mid="white",
  #high="darkred", midpoint = 0) + #Use scale_fill_gradient2 if you want two color gradients
  scale_fill_gradientn(colors = c("darkblue", "white", "darkred"), 
                       values = rescale(c(min(data$t_diff), 0, max(data$t_diff))), 
                       limits = c(min(data$t_diff), max(data$t_diff))) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "black")) +
  labs(title = "Global temperatures have increased by over 1.2\u00B0C since 1880") + 
  theme(plot.title = element_text(color = "white", hjust=0.1))

# Recreating line plot for temperature anomalies ----

t_diff <- read.csv("data/GLB.Ts+dSST.csv",skip = 1, na="***") %>% 
  select(year = Year, month.abb) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  na.exclude() %>% 
  mutate(month = factor(month, levels = month.abb), 
         this_year = year == 2022) #to organize January to December in the plotz

next_Jan <- t_diff %>% 
  filter(month == "Jan") %>% 
  mutate(year = year - 1, month ="next_Jan")

last_Dec <- t_diff %>% 
  filter(month == "Dec") %>% 
  mutate(year = year + 1, month ="last_Dec")

t_diff <- bind_rows(last_Dec, t_diff, next_Jan) %>% 
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1, 
         this_year = year == 2022) 

annotation <- t_diff %>% 
  slice_max(year) %>% 
  slice_max(month)

t_diff %>% 
  ggplot(aes(month_number, t_diff, group=year, color=year, size = this_year)) + # added size to track "this_year" changes fron annotation
  geom_line() + 
  geom_hline(yintercept = 0, color = "red", size = 0.7) + # adding red horizontal line indicating zero
  geom_text(data=annotation, aes(x=month_number, y=t_diff, label=year, color=year), 
            inherit.aes=FALSE,
            hjust=0, size=5) + 
  scale_x_continuous(breaks=1:12, 
                     labels = month.abb) + # To label month number by month name
  coord_cartesian(xlim=c(1,12)) + # to be able to fill the gaps both sides of line graph
  scale_color_viridis_c(breaks=seq(1880,2020,20)) + 
  scale_size_manual(breaks=c(FALSE,TRUE), 
                    values=c(0.25,1), 
                    guide="none") + # adjust the values of the color bar
  labs(x = "By Month", 
       y = "Temperature change since Pre-Industrial Time \u00B0C", 
       title = "Global Temperature Change since 1880") + 
  theme(panel.background = element_rect(fill="black"), 
        panel.grid = element_blank(),
        plot.title = element_text(color = "white", face = "bold"), 
        plot.background = element_rect(fill="#444444"), 
        axis.text = element_text(color="White"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        legend.title = element_blank(), 
        legend.background = element_rect(fill = NA),  
        legend.text = element_text(color = "white"), 
        legend.key.height = unit(55, "pt"), 
  )

# Making a spiral climate temperature ---- 

# This is just based on the previous plot 

t_diff <- read.csv("data/GLB.Ts+dSST.csv",skip = 1, na="***") %>% 
  select(year = Year, month.abb) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% # According to the year, rename to month, and distribute values from this dataset
  na.exclude() %>% 
  mutate(month = factor(month, levels = month.abb), # Setting factor levels to constant values from month.abb
         this_year = year == 2022) #to organize January to December in the plot

next_Jan <- t_diff %>% 
  filter(month == "Jan") %>% 
  mutate(year = year - 1, month ="next_Jan")

last_Dec <- t_diff %>% 
  filter(month == "Dec") %>% 
  mutate(year = year + 1, month ="last_Dec")

t_diff <- bind_rows(last_Dec, t_diff, next_Jan) %>% 
  mutate(month = factor(month, levels = c( month.abb, "next_Jan")), # Setting factor levels to constant values from month.abb INCLUDING "next_Jan"
         month_number = as.numeric(month) - 1,
         this_year = year == 2022
         ) 

annotation <- t_diff %>% 
  slice_max(year) %>% 
  slice_max(month)

temp_line <- tibble(
  x = 12, 
  y = c(1.5, 2.0), 
  labels = c("1.5\u00B0C", "2\u00B0C")
)

month_labels <- tibble(
    x = 1:12, 
    labels = month.abb, 
    y = 2.4
  )

t_diff %>% 
  ggplot(aes(month_number, t_diff, group=year, color=year, size = this_year)) + # added size to track "this_year" changes fron annotation
  geom_hline(yintercept = c(1.5, 2.0), color = "red", size = 0.7) + # adding red horizontal line indicating zero
  geom_label(data = temp_line, aes(x=x, y=y, label=labels),
             color = "red", fill = "black", label.size = 0,
             inherit.aes = FALSE) +
  geom_line() + 
  geom_text(data = month_labels, aes(x=x, y=y,label=labels), 
            color = "white",
            angle = seq(360 - 360/12, 0, length.out = 12), 
            inherit.aes = FALSE) + #in order to tangential month labels
  # geom_text(data=annotation, aes(x=month_number, y=t_diff, label=year, color=year), 
  #           inherit.aes=FALSE,
  #           hjust=0, size=5) + 
  geom_point(data = annotation, 
             aes(x=month_number, y=t_diff, color = year), 
             size = 3) +  # To  add point at present time
  scale_x_continuous(breaks=1:12, 
                     labels = month.abb) + # To label month number by month name
  coord_polar() + 
  scale_color_viridis_c(breaks=seq(1880,2020,20), 
                        guide = "none") + 
  scale_size_manual(breaks=c(FALSE,TRUE), 
                    values=c(0.25,1), 
                    guide="none"
                    ) + # adjust the values of the color bar
  labs(x = "By Month", 
       y = "Temperature change since Pre-Industrial Time \u00B0C", 
       title = "Global Temperature Change since 1880") + 
  theme(panel.background = element_rect(fill="black"), 
        panel.grid = element_blank(),
        plot.title = element_text(color = "white", face = "bold"), 
        plot.background = element_rect(fill="#444444"), 
        axis.text = element_text(color="White"),
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(colour = "white"))

# Creating animation for spiral climate temperature ---- 

t_diff <- read.csv("data/GLB.Ts+dSST.csv",skip = 1, na="***") %>% 
  select(year = Year, month.abb) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% # According to the year, rename to month, and distribute values from this dataset
  na.exclude() %>% 
  mutate(month = factor(month, levels = month.abb),
         year = as.integer(year), 
         month_number = as.numeric(month)
         ) %>% 
  arrange(year, month)

tail(t_diff)

next_Jan <- t_diff %>% 
  filter(month == "Jan") %>% 
  mutate(year = year - 1, month ="next_Jan")

last_Dec <- t_diff %>% 
  filter(month == "Dec") %>% 
  mutate(year = year + 1, month ="last_Dec")

t_diff <- bind_rows(last_Dec, t_diff, next_Jan) %>% 
  mutate(month = factor(month, levels = c( month.abb, "next_Jan")), # Setting factor levels to constant values from month.abb INCLUDING "next_Jan"
         month_number = as.numeric(month) - 1,
         this_year = year == 2022, 
         mutate(step_number = 1:nrow(.))) +  
annotation <- t_diff %>% 
  slice_max(year) %>% 
  slice_max(month)

temp_line <- tibble(
  x = 12, 
  y = c(1.5, 2.0), 
  labels = c("1.5\u00B0C", "2\u00B0C")
)

month_labels <- tibble(
  x = 1:12, 
  labels = month.abb, 
  y = 2.4
)

t_diff %>% 
  ggplot(aes(month_number, t_diff, group=year, color=year)) + # added size to track "this_year" changes fron annotation
  geom_hline(yintercept = c(1.5, 2.0), color = "red", size = 0.7) + # adding red horizontal line indicating zero
  geom_label(data = temp_line, aes(x=x, y=y, label=labels),
             color = "red", fill = "black", label.size = 0,
             inherit.aes = FALSE) +
  geom_line() + 
  geom_text(data = month_labels, aes(x=x, y=y,label=labels),
            color = "white",
            length.out = 12,
            inherit.aes = FALSE) + #month labels around the angle
  geom_text(aes(x=1, y=-1.3, label=year), 
            color = "white",
            inherit.aes=FALSE,
            hjust=0.5, size=5) + # year label at the center of the circle
  # geom_point(data = annotation, 
  #            aes(x=month_number, y=t_diff, color = year), 
  #            size = 3) +  # To  add point at present time
  scale_x_continuous(breaks=1:12, 
                     labels = month.abb) + # To label month number by month name
  coord_polar() + 
  scale_color_viridis_c(breaks=seq(1880,2020,20), 
                        guide = "none") + 
  scale_size_manual(breaks=c(FALSE,TRUE), 
                    values=c(0.25,1), 
                    guide="none"
  ) + # adjust the values of the color bar
  labs(x = "By Month", 
       y = "Temperature change since Pre-Industrial Time \u00B0C", 
       title = "Global Temperature Change since 1880") + 
  theme(panel.background = element_rect(fill="black"), 
        panel.grid = element_blank(),
        plot.title = element_text(color = "white", face = "bold"), 
        plot.background = element_rect(fill="#444444"), 
        axis.text = element_text(color="White"),
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(colour = "white")) +
  transition_reveal(along = step_number)

# Creating the NASA GISS animated climate spiral ---- 

t_diff <- read.csv("data/GLB.Ts+dSST.csv",skip = 1, na="***") %>% 
  select(year = Year, month.abb) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% # According to the year, rename to month, and distribute values from this dataset
  na.exclude() %>% 
  mutate(month = factor(month, levels = month.abb),
         year = as.integer(year), 
         month_number = as.numeric(month)
  ) %>% 
  arrange(year, month)

tail(t_diff)

next_Jan <- t_diff %>% 
  filter(month == "Jan") %>% 
  mutate(year = year - 1, month ="next_Jan")

# last_Dec <- t_diff %>% 
#   filter(month == "Dec") %>% 
#   mutate(year = year + 1, month ="last_Dec")

t_diff <- bind_rows(t_diff, next_Jan) %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>%
  arrange(year, month) %>%
  filter(year != 1879) %>%
  mutate(step_number = 1:nrow(.))
  
annotation <- t_diff %>% 
  slice_max(year) %>% 
  slice_max(month)

temp_line <- tibble(
  x = 1,
  y = c(1, 0, -1), 
  labels = c("+1\u00B0 C","0\u00B0 C", "-1\u00B0 C"))

month_labels <- tibble(
  x = 1:12, 
  labels = toupper(month.abb), 
  y = 1.5
)

gridlines <- tibble(
  x = 1.5,
  xend = 12.5, 
  y = c(1, 0, -1), 
  yend = c(1, 0, -1)
)

t_diff_circular <- t_diff %>% 
  ggplot(aes(month_number, t_diff, group=year, color=t_diff)) + # added size to track "this_year" changes fron annotation
  geom_line() + 
  geom_segment(data = gridlines, 
               aes(x=x, y=y, xend =xend, yend=yend), 
               color = c("yellow", "green", "yellow"), 
               inherit.aes = FALSE) + # adding red horizontal line indicating zero
  geom_text(data = temp_line, aes(x=x, y=y, label=labels),
             color = c("yellow", "green", "yellow"), 
            fill = "black", label.size = 0,
             inherit.aes = FALSE) +
  geom_text(data = month_labels, aes(x=x, y=y,label=labels),
            color = "yellow",
            length.out = 12,
            inherit.aes = FALSE) + #month labels around the angle
  geom_text(aes(x=1, y=-1.3, label=year), 
            color = "white",
            inherit.aes=FALSE,
            hjust=0.5, size=5) + # year label at the center of the circle
  # geom_point(data = annotation, 
  #            aes(x=month_number, y=t_diff, color = year), 
  #            size = 3) +  # To  add point at present time
  scale_x_continuous(breaks=1:12, 
                     labels = month.abb) + # To label month number by month name
  # scale_y_continuous(limits = c(-2.0,1.5)) + 
  coord_polar(start = 0) + # rearrange month labels to start with 1 (Jan)
  scale_color_gradient2(low = "dodgerblue4", high = "firebrick", mid = "lavenderblush", midpoint = 0, 
                        guide = "none") + #minimum color >> middle value >> higher number 
  scale_size_manual(breaks=c(FALSE,TRUE), 
                    values=c(0.25,1), 
                    guide="none"
  ) + # adjust the values of the color bar
  # labs(x = "By Month", 
  #      y = "Temperature change since Pre-Industrial Time \u00B0C", 
  #      title = "Global Temperature Change since 1880") + 
  theme(panel.background = element_rect(fill="black"), 
        panel.grid = element_blank(),
        plot.title = element_text(color = "white", face = "bold"), 
        plot.background = element_rect(fill="black"), 
        axis.text = element_text(color="White"),
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_blank())

animation <- t_diff_circular + transition_reveal(along = step_number)
animate(animation, end_pause = 10,fps = 7)
