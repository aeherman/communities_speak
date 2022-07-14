library(tidyverse)

# df: the dataframe
# by_vars: generally the demographic variables, or the denominator by which we examine the variable of the hypothesis
# hyp_var: the hypothesis variable (must be in logical format)
# min: the minimum number of successes a category must have
# conf: the confidence interval
# title: the title of the plo
# show: currently nonfunctional argument that I plan to use to allow more flexibility in showng plots with nonsignificant relationships

# by_vars -> "by"
make_plots <- function(df, by_vars, hyp_var, min = 5, conf = 0.01,
                       title = "Title", show = NULL,
                       codebook = read_csv(file.path(root, "data/output/codebook.csv"),
                                           show_col_types = FALSE)) {
  
  
  hyp_var_labels <- codebook %>% filter(variable %in% hyp_var) %>% select(variable, description) %>%
    mutate(hyp_var_label = glue::glue("{variable}: {description}")) %>% select(variable, hyp_var_label)
  
  #sym_var <- sym(hyp_var)
  
  #p <- mean(df[[hyp_var]], na.rm = TRUE)
  #q <- 1 - p
  #sym_var <- sym(var)
  out <- lapply(setNames(by_vars, by_vars), function(item){
    sym_item <- sym(item)
    reshaped <- df %>%
      group_by_at(vars(!!sym_item)) %>%
      summarize(across(all_of(hyp_var), list(~sum(., na.rm = TRUE),
                                             ~sum(!is.na(.)),
                                             ~mean(., na.rm = TRUE)))) %>%
      pivot_longer(cols = -!!sym_item) %>%
      separate(name, into = c("hyp_var", "stat"), sep = "\\_(?=\\d)") %>%
      mutate(stat = str_replace_all(stat, c("1" = "n", "2" = "denom", "3" = "prop"))) %>%
      pivot_wider(id_cols = c(!!sym_item, "hyp_var"), names_from = stat) %>%
      na.omit %>%
      # translate numbers to labels
      mutate_if(labelled::is.labelled, labelled::to_character) %>%
      # replace long labels with readable shorter values
      mutate(across(!!sym_item,
                    ~str_replace_all(.,
                                    c("transgender.*" = "transgender",
                                      "indigenous.*" = "Indigenous American")))
             ) %>%
      left_join(hyp_var_labels, by = c("hyp_var" = "variable")) %>%
      arrange(hyp_var, prop)
    
    filtered <- reshaped %>% filter(if_all(starts_with("denom"), ~.>=min)) #%>% select(-!!sym_var, -width)
    
    p.values <- lapply(setNames(hyp_var, hyp_var), function(var){
      sub <- filtered %>% filter(hyp_var == var)
      cats <- sub[[item]]
      
      #p.values <- c()
      p.values_table <- matrix(ncol = length(cats), nrow = length(cats))
      colnames(p.values_table) <- cats
      rownames(p.values_table) <- cats
      i <- 1
      
      for(cat1 in cats) {
        for(cat2 in cats) {
          if(cat1 == cat2) {
            next
          }
          temp <- sub[cats == cat1,][c("n", "denom")] %>%
            rbind(sub[cats == cat2,][c("n", "denom")]) %>%
            mutate(p = sum(n)/sum(denom),
                   q = 1 - p,
                   denom_p = denom*p,
                   denom_q = denom*q) %>% filter(if_all(starts_with("denom"), ~.>=5))
          
          if(nrow(temp) <= 1) {
            next
          }
          
          #name <- paste(sort(c(cat, cat2), collapse = ' & ')
          p.value <- signif(prop.test(temp$n, temp$denom)$p.value, 2)
          #attributes(p.value) <- list(warning = warnings())
          #names(p.value) <- glue::glue("{cat} & {cat2} p-value:")
          
          if(any(
            #glue::glue("{cat2} & {cat} p-value:") %in% names(p.values),
            p.value > conf,
            is.na(p.value))) {
            next
          }
          p.values_table[cat1, cat2] <- p.value
          #p.values <- c(p.values, p.value)
          #print(glue::glue("{i}: {cat}, {cat2}"))
          #i <- i+1
        }
      }
      return(p.values_table)
    })
    
    legend_labels_df <- codebook %>% filter(variable %in% hyp_var) %>% select(variable, description)
    legend_labels <- legend_labels_df$description[match(hyp_var, legend_labels_df$variable)]
    hyp_legend_labels <- sort(paste(hyp_var, legend_labels, sep = ": "))
    
    # return plots that have at least one statistically significant value
    if(all(is.na(unlist(p.values))) & is.null(show) | is.null(p.values) & is.null(show) & length(hyp_var) == 1) {
      return(NULL)
    } else {
      plot <- filtered %>% #filter(n >= min) %>%
        # plot
        ggplot(aes(x = prop,
                   y = reorder(stringr::str_to_title(labelled::to_character(!!sym_item)),
                               -prop),
                   group = hyp_var_label,
                   fill = hyp_var_label)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = glue::glue("{signif(prop, 2)*100}% {n}/{denom}")),
                  # vertical
                  #hjust = 0.5, vjust = -0.5,
                  #position = position_dodge(width = 0.9),
                  # horizontal
                  hjust = -0.1,
                  position = position_dodge(width = 0.9),
                  size = 3) + 
      
      
        #geom_vline(xintercept = p, lty = "dashed", color = project_pal[1]) +
        #geom_col(aes(x = p, y = "All")) +
        
        # colors
        #coord_flip() +
        
        scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
        scale_fill_manual(NULL, values = project_pal) +
        #scale_color_manual(NULL, values = project_pal) +
        project_theme + # question: add a variable for this?
        
        # labels
        xlab(NULL) + ylab(NULL) +
        ggtitle(glue::glue("{stringr::str_to_title(title)} by {stringr::str_to_title(item)}")) +
        
        theme(
          plot.subtitle = element_text(size = 8),
          plot.title = element_text(size = 10),
          plot.caption = element_text(size = 7),
          axis.text.x = element_text(size = 8),
          legend.position = "bottom",
          legend.direction = "vertical"
        ) + guides(fill = guide_legend(reverse = TRUE))
        
        if(min(reshaped$denom) < min) {
          pulled <- reshaped %>% filter(denom < min) %>%
            mutate_if(labelled::is.labelled, labelled::to_character) %>%
            pull(!!sym_item) %>% unique
          cats <- #glue::glue("'{pulled}'") %>%
            paste(pulled, collapse = ", ")
          plot <- plot +
            labs(caption = glue::glue("*Categories with less than {min} responses excluded: '{cats}'"))
        }
      
      #if(any(str_length(reshaped[[item]]) >= 20)) {
      #  plot <- plot + theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))
      #}
      
      if(length(hyp_var) == 1) {
        plot <- plot + theme(legend.position = "none")
      }
        
    }
    return(list(plot = plot, p.values = p.values))
  })
  return(out)
}
