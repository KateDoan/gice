plotPrev <- function(g, ylab, ylim, text = "NEVER USERS", leg = FALSE) {
    df_prop <- df_prop %>% filter(scenario %in% chosen)
    p <- ggplot(df_prop, aes_string(x="year", y=g, group="scenario",
                                    col="scenario", shape="scenario", linetype="scenario", fill="scenario")) +
        theme_classic() +
        theme(panel.border=element_rect(color = foregroundCol, fill=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = panelCol),
              plot.background = element_rect(fill = backgroundCol)) +
        theme(axis.text.x  = element_text(color=foregroundCol, size=axisTextSize),
              axis.text.y = element_text(color=foregroundCol, size=axisTextSize),
              axis.title.x = element_text(color=foregroundCol, size=axisTextSize, face="bold"),
              axis.title.y = element_text(color=foregroundCol, size = axisTextSize, face="bold"),
              axis.ticks = element_line(color=foregroundCol),
              axis.line = element_line(colour=foregroundCol)) +
        annotation_custom(grobTree(textGrob(text, x=unit(0.5, 'lines'),  y=unit(1,'npc')+unit(-0.6, 'lines'), 
                                            hjust=0, gp=gpar(fontface="bold", fontsize=annoteTextSize, col=foregroundCol))))+
        geom_line() +
        geom_point(data=df_prop%>%filter(year %in% CHOSEN_TIMES))+
        scale_linetype_manual(values = LINETYPES,
                              name = " ",
                              limits = unique(SCENARIOS),
                              labels = LABELS) +
        scale_shape_manual(values = SHAPES,
                           name = " ",
                           limits = unique(SCENARIOS),
                           labels = LABELS) +
        scale_fill_manual(values = COLZ,
                          name = " ",
                          limits = SCENARIOS,
                          labels = LABELS) +
        scale_color_manual(values = COLZ,
                           name = " ",
                           limits = SCENARIOS,
                           labels = LABELS) +
        theme(legend.position = "none") +
        labs(x="Year", y=ylab) +
        ylim(ylim) 
    
    if(leg)
        p <- p + theme(legend.position = c(x=unit(0, "lines"), y=unit(0, "lines")),
                       legend.justification = c(0, 0),
                       legend.background = element_rect(fill=NA),
                       legend.title = element_blank(),
                       legend.text = element_text(size=legendTextSize, colour=foregroundCol),
                       legend.key.size = unit(0.8, "lines"))
    
    if(g=='CandD')
        p <- p + theme(plot.margin = unit(c(6,6,6,9), "pt"))
    
    p

}