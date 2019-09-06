library(ggplot2)
library(readr)
library(gridExtra)

##########################################################################################
# In and Out directories
inDir <- "data/singapore"
outDir <- "code/figures/other_figures/plots"
##########################################################################################
# Helper function
abs_comma <- function (x, ...) {
    format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}
##########################################################################################
# Read data in
singpop19922017 <- read_csv(file.path(inDir,"singpop_19922017.csv"))
View(singpop19922017)
singpop19922017 <- singpop19922017 %>% mutate(number = as.numeric(number),
                                              category = factor(category, levels = unique(category)))

p1 <- ggplot(data = singpop19922017, aes(x = category, y = number)) +
    geom_bar(data =  singpop19922017%>% filter(gender == "female", year == 1992),
             stat = "identity",
             position = "identity",
             fill = "orange") +
    geom_bar(data = singpop19922017 %>% filter(gender == "male", year == 1992),
             stat = "identity",
             position = "identity",
             mapping = aes(y = -number),
             fill = "orange") +
    coord_flip() +
    theme_classic() +
    scale_y_continuous(labels = abs_comma) +
    theme(axis.title.y = element_blank()) +
    labs(y = "Singapore population in 1992")
    

p2 <- ggplot(data = singpop19922017, aes(x = category, y = number)) +
    geom_bar(data =  singpop19922017%>% filter(gender == "female", year == 2017),
             stat = "identity",
             position = "identity",
             fill = "deepskyblue") +
    geom_bar(data = singpop19922017 %>% filter(gender == "male", year == 2017),
             stat = "identity",
             position = "identity",
             mapping = aes(y = -number),
             fill = "deepskyblue") +
    coord_flip() +
    theme_classic() +
    scale_y_continuous(labels = abs_comma) +
    scale_x_discrete(position = "top") +
    theme(axis.title.y = element_blank()) +
    labs(y = "Singapore population in 2017")

png(file.path(outDir,'sin_pyramid.png'),height=8,width=15,units='cm',res=400,pointsize=10)
grid.arrange(p1, p2, nrow = 1)
dev.off()
    
