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

# Converting month and temp data to polar coordinates ---- 

library(tidyverse)
library(gganimate)

t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, month.abb) %>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

# next_jan <- t_diff %>%
#   filter(month == "Jan") %>%
#   mutate(year = year - 1,
#          month = "next_Jan")

radius_bump <- 1.5 # to avoid t_diff from becoming negative. No negative changes


t_data <- t_diff %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(step_number = 1:nrow(.),
         radius = t_diff + radius_bump,
         theta = 2 * pi * (month_number - 1) / 12, #Jan starts at 0, December at 11 
                                                   #Need to convert into radiance 2*pi
         x = radius * sin(theta),
         y = radius * cos(theta))

annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)

temp_lines <- tibble(
  x = 0,
  y = c(1, 0, -1) + radius_bump,
  labels = c("+1\u00B0 C", "0\u00B0 C", "-1\u00B0 C")
)

month_labels <- tibble(
  theta = 2*pi*(1:12 - 1)/12,
  radius = 1.5 + radius_bump, 
  labels = toupper(month.abb),
  x = radius * sin(theta),
  y = radius * cos(theta)
)

# gridlines <- tibble(
#   x = c(1.2, 1.3, 1.6),
#   xend = c(12.8, 12.7, 12.4),
#   y = c(1, 0, -1), 
#   yend = y
# )

gridlines <- tibble(theta = rep(2*pi * seq(0, 1, 0.01), each = 3),
                    radius = rep(c(1, 0, -1) + radius_bump, length.out = length(theta)),
                    line = rep(c("a", "b", "c"), length.out = length(theta)),
                    x = radius * sin(theta),
                    y = radius * cos(theta)) %>%
  filter((line == "a" & theta > 0.01 * 2 *pi & theta < 0.99 *2 *pi) |
           (line == "b" & theta > 0.025 * 2 *pi & theta < 0.975 *2 *pi) |
           (line == "c" & theta > 0.05 * 2 *pi & theta < 0.95 *2 *pi))

a <- t_data %>% 
  ggplot(aes(x=x, y=y, color=t_diff)) +
  geom_label(aes(x = 0, y= 0, label = year), #x and y is 0 to center the year label
             fill="black",
             label.size = 0,
             size=6) +
  geom_path() +
  geom_path(data=gridlines %>% filter(radius != radius_bump),
            aes(x=x, y=y, group = line),
            color="yellow",
            inherit.aes = FALSE) +
  geom_path(data=gridlines %>% filter(radius == radius_bump),
            aes(x=x, y=y, group = line),
            color="green",
            inherit.aes = FALSE) +
  geom_text(data = temp_lines, aes(x=x, y=y, label=labels),
            color=c("yellow", "green", "yellow"), size=2, fontface="bold",
            inherit.aes=FALSE) +
  geom_text(data = month_labels, aes(x=x, y=y, label = labels),
            inherit.aes = FALSE, color="yellow"
  ) +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, -0.3)) +
  scale_x_continuous(limits = c(-4, 4), expand = c(0, -0.3)) +
  coord_fixed() +
  scale_color_gradient2(low = "blue", high = "red", mid="white", midpoint = 0,
                        guide = "none") +
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill = "black", color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
  ) +
  transition_reveal(step_number)

ggsave("figures/climate_spiral_trig.png", width=4.5, height=4.5, unit="in")

animate(a, width=4.5, height=4.5, unit="in", res=300)
anim_save("figures/climate_spiral_trig.gif")
# 
# 
# animate(a, width=4.155, height=4.5, unit="in", res=300,
#         renderer = av_renderer("figures/climate_spiral_trig.mp4")
#         )


# 3D climate spiral ----


library(plotly)
library(glue) #to call for GLUE command and concatenate different datasets
library(htmlwidgets)


radius_bump <- 1.5

t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, month.abb) %>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

t_data <- t_diff %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(step_number = 1:nrow(.),
         radius = t_diff + radius_bump,
         theta = 2 * pi * (month_number - 1) / 12, #Jan starts at 0, December at 11 
         #Need to convert into radiance 2*pi
         x = radius * sin(theta),
         y = radius * cos(theta), 
         z = year, 
         label = glue("{month} {year}\n{t_diff}\u00B0 C"))

t_data %>% 
  ggplot(aes(x=x, y=y, color = year)) + 
  geom_path()


count <- 3000

x <- c()
y <- c()
z <- c()
c <- c()

for (i in 1:count) {
  r <- i * (count - i)
  x <- c(x, r * cos(i / 30))
  y <- c(y, r * sin(i / 30))
  z <- c(z, i)
  c <- c(c, i)
} 

data <- data.frame(x, y, z, c)


axx <- list(
  title = "", 
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE
)

axy <- list(
  title = "", 
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE
)

axz <- list(
  title = ""
)


fig <- plot_ly(t_data, x = ~x, y = ~y, z = ~z,
        text = ~label, 
        hoverinfo = 'text',
        type = 'scatter3d', 
        mode = 'lines',
        line = list(width = 4, color = ~t_diff,
                    cmid = 0,
                    colorscale = list(c(0,'blue'), 
                                      c(0.5,'white'), 
                                      c(1,'red'))))

p <- fig %>% 
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

saveWidget(p, "figures/climate_spiral_plotly.html")









