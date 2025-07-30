library(tidyverse)
library(dplyr)
library(ggpubr) 
library(ggplot2)
library(rstatix)

#load example data set 
rm(list=ls())
df <- read.csv("example_data.csv")

feature_list = colnames(df[7:(length(df))])

## New column to group timepoints into pre and post
values <- c("pre", "post")
index <- c(1, 4)
df$Timepoint <- values[match(df$`Week`, index)]

#order variables
df$Timepoint <- factor(df$Timepoint, levels = c("pre", "post"))

#write function
run_wilcoxon <- function(dataframe, time_col, feature_cols, paired = TRUE) {
    
    ## Time point can be either pre/post for paired data or other grouping (treatment group?)
    ## paired set to true, option to include 'paired = FALSE' as a fourth argument for the function
    
    pvalues <- c()
    med_pre <- c()
    med_post <- c()
    
    for (feature in feature_cols) {
        # Ensure numeric
        dataframe[[feature]] <- as.numeric(dataframe[[feature]])
        
        # Get medians
        medians <- aggregate(dataframe[[feature]] ~ dataframe[[time_col]], dataframe, median)
        medians <- setNames(medians[,2], medians[,1])
        
        med_pre <- c(med_pre, medians["pre"])
        med_post <- c(med_post, medians["post"])
        
        # Wilcoxon test
        p <- wilcox.test(dataframe[[feature]] ~ dataframe$Timepoint, paired = paired)$p.value
        pvalues <- c(pvalues, p)
    }
    
    return(data.frame(
        feature = feature_cols,
        p_value = pvalues,
        median_pre = med_pre,
        median_post = med_post
    ))
}

results <- run_wilcoxon(df, "Timepoint", feature_list)

write.csv(results, "./Wilcoxon_Test_Output.csv", row.names=FALSE)


#create boxplots 
create_boxplots <- function(dataframe, feature_cols, group_col, paired = TRUE) {
    plots_list <- list()
    
    my_comparisons <- list(c("pre", "post")) ## Can change to what is in the group_col
    
    for (feature in feature_cols) {
        dataframe[[feature]] <- as.numeric(dataframe[[feature]])
        
        plot <- ggplot(dataframe, aes(x = factor(.data[[group_col]]), y = .data[[feature]], color=Timepoint)) +
            geom_boxplot() +
            theme_bw() +
            xlab("") +
            ylab("units") +
            ggtitle(feature) + 
            stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=TRUE, size=3,
                                                  step.increase = .075, tip.length = 0.01, hide.ns = TRUE) +
            geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) +
            theme(legend.position = "none")
        
        plots_list[[feature]] <- plot
    }
    
    return(plots_list)
}

to_plot <- create_boxplots(df, feature_list, "Timepoint")


#save plots
save_plots_pdf <- function(plots_list, output_path) {
    pdf(output_path)
    
    plot_num <- 1
    plot_names <- names(plots_list)
    
    while (plot_num <= length(plots_list)) {
        plots_subset <- plots_list[plot_names[plot_num:min(plot_num + 3, length(plots_list))]]
        print(do.call(aplot::plot_list, c(unname(plots_subset), nrow = 2, ncol = 2)))
        plot_num <- plot_num + 4
        print(plot_num)
    }
    
    dev.off()
}

save_plots_pdf(to_plot, "Wilcoxon_Test_Output_BoxPlots.pdf")

