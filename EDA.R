# Load required libraries
library(ggplot2); library(dplyr); library(RSvgDevice)

#######################
# Load all clean data #
#######################

files <- dir("clean_data/")

clean_data <-  data.frame()

for (i in files){
    print(paste("clean_data/", i, sep=""))
    rds <- readRDS(paste("clean_data/", i, sep=""))
    clean_data <- rbind(clean_data, rds)
}

# Remove all duplicates
dup <- duplicated(clean_data[,2])
clean_data <- clean_data[!dup,]

rm(i, rds, files, dup)

##############
# HISTOGRAMS #
##############

# SIZE

ggplot(clean_data, aes(x = size, fill = type)) +
    geom_histogram(binwidth=2.5, color="white") +
    coord_cartesian(xlim = c(quantile(clean_data$size, 0.025),
                             quantile(clean_data$size, 0.975))) +
    scale_fill_manual(breaks=c("apartment","terrace","semi-detached", "single house"),
                      values=c("#7D4F7D", "#2EA197", "#D19C49", "#D14A41"),
                      name="Housing type   ") +
    theme(legend.position="bottom",
          legend.title = element_text(colour="#3F4E59", size=16),
          legend.text = element_text(colour="#3F4E59", size = 14),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey")) +
    xlab(expression(paste("size ", m^2, sep="")))


# PRICE

ggplot(clean_data, aes(x = price, fill = type)) +
    geom_histogram(binwidth=50, color="white") +
    coord_cartesian(xlim = c(quantile(clean_data$price, 0.025),
                             quantile(clean_data$price, 0.975))) +
    scale_fill_manual(breaks=c("apartment","terrace","semi-detached", "single house"),
                      values=c("#7D4F7D", "#2EA197", "#D19C49", "#D14A41")) +
    theme(legend.position="none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey")) +
    xlab("price EUR")


#############################
# SCATTER PLOT PRICE ~ SIZE #
#############################

ggplot(clean_data, aes(x = size, y = price, color=city)) +
    geom_point() +
    coord_cartesian(xlim = c(quantile(clean_data$size, 0.025),
                             quantile(clean_data$size, 0.975)),
                    ylim = c(quantile(clean_data$price, 0.025),
                             quantile(clean_data$price, 0.975)))


#########################
# Price by type by city #
#########################

ggplot(clean_data,
       aes(x = city, y = price, color = city)) +
    geom_boxplot(outlier.colour = "white") +
    geom_point(alpha = 0.2) +
    coord_cartesian(ylim = c(quantile(clean_data$price, 0.025),
                             quantile(clean_data$price, 0.975))) +
    theme(legend.position="none",
          legend.title = element_text(colour="#3F4E59", size=16),
          legend.text = element_text(colour="#3F4E59", size = 14),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey")) +
    xlab("") +
    ylab("price EUR")


###############
# CITY GROUPS #
###############

city_group <- subset(clean_data, po_code != "", select=c(city, price_sqm, type)) %>%
    group_by(city) %>%
    summarise(mean_price_sqm = mean(price_sqm),
              n = n())

mean_values = tapply(city_group$n, as.factor(city_group$city), mean)


barplot(city_group$n, names.arg = c("ESPOO", "HELSINKI", "VANTAA"),
        col = c("#E56AD0", "#68b3f6", "#61dd45"), border = NA, density = 20,
        axes = FALSE)


# Heatmap Type ~ City
city_group <- subset(clean_data, po_code != "", select=c(city, price_sqm, type)) %>%
    group_by(city, type) %>%
    summarise(mean_price_sqm = mean(price_sqm),
              n = n())

ggplot(city_group, aes(x = type, y = city)) +
    geom_tile(aes(fill = mean_price_sqm), colour = "white") +
    scale_fill_gradient(low = "#2EA197", high = "#D14A41", 
                        name=expression(paste("Average price\n EUR /", m^2, sep=""))) +
    theme(legend.title = element_text(colour="#3F4E59", size=16),
          legend.text = element_text(colour="#3F4E59", size = 14),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "white")) +
    xlab("") +
    ylab("") +
    ggtitle("by dwelling type")

# Check the hot points, it may be that there are few or even just one observation, which makes it
# not especially representative of the average price per square meter
# sum(clean_data$city == "Espoo" & clean_data$type == "single house")

##########
# AGENTS #
##########

# Plot number of rentals per agent
ggplot(clean_data, aes(x = agent)) +
    geom_bar(position = "dodge", fill="#2EA197") +
    coord_flip() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(colour = "grey")) +
    xlab("Real state agents") +
    ylab("Amount of rentals")


# Create group
agent_group <- subset(clean_data, po_code != "", select=c(city, price_sqm, type, agent)) %>%
    group_by(agent, city, type) %>%
    summarise(mean_price_sqm = mean(price_sqm),
              n = n())

ggplot(agent_group, aes(y = mean_price_sqm, x = agent)) +
    geom_boxplot() +
    coord_flip()

ggplot(agent_group, aes(x = type, y = agent)) +
    geom_tile(aes(fill = mean_price_sqm), colour = "white") +
    scale_fill_gradient(low = "#2EA197", high = "#D14A41",
                        name=expression(paste("Average price\n EUR  /", m^2, sep=""))) +
    theme(legend.position="bottom",
          legend.title = element_text(colour="#3F4E59", size=16),
          legend.text = element_text(colour="#3F4E59", size = 14),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "white")) +
    xlab("") +
    ylab("") +
    ggtitle("by dwelling type")

# Check the hot points, it may be that there are few or even just one observation, which makes it
# not especially representative of the average price per square meter for that given real state agency
# sum(clean_data$agent == "Vuokrahuone" & clean_data$type == "apartment")

ggplot(agent_group, aes(x = city, y = agent)) +
    geom_tile(aes(fill = mean_price_sqm), colour = "white") +
    scale_fill_gradient(low = "#2EA197", high = "#D14A41",
                        name=expression(paste("Average price\n EUR  /", m^2, sep=""))) +
    theme(legend.position="bottom",
        legend.title = element_text(colour="#3F4E59", size=16),
          legend.text = element_text(colour="#3F4E59", size = 14),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "white")) +
    xlab("") +
    ylab("") +
    ggtitle("by city")

# Check the hot points, it may be that there are few or even just one observation, which makes it
# not especially representative of the average price per square meter for that given real state agency
# sum(clean_data$agent == "Vuokrahuone" & clean_data$city == "Helsinki")