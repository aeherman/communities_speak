library(tidyverse)

# df: the dataframe
# by_vars: generally the demographic variables, or the denominator by which we examine the variable of the hypothesis
# hyp_var: the hypothesis variable (must be in logical format)
# min: the minimum number of successes a category must have
# conf: the confidence interval
# title: the title of the plo
# show: currently nonfunctional argument that I plan to use to allow more flexibility in showng plots with nonsignificant relationships


# by_vars -> "by"
make_plots <- function(df, by_vars, hyp_var, min = 5, conf = 0.1,
                       title = "Title", show = NULL) {
  
  sym_var <- sym(hyp_var)
  
  p <- mean(df[[hyp_var]], na.rm = TRUE)
  q <- 1 - p
  #sym_var <- sym(var)
  out <- lapply(setNames(by_vars, by_vars), function(item){
    sym_item <- sym(item)
    reshaped <- df %>%
      group_by_at(vars(!!sym_item)) %>%
      summarize(n = sum(!!sym_var, na.rm = TRUE),
                denom = sum(!is.na(!!sym_var)),
                prop = mean(!!sym_var, na.rm = TRUE)) %>%
      na.omit %>% mutate_if(labelled::is.labelled, labelled::to_character) #%>%
      #mutate(denom_q = denom*q, denom_p = denom*p)
    
    filtered <- reshaped %>% filter(if_all(starts_with("denom"), ~.>=min)) #%>% select(-!!sym_var, -width)
    
    p.values <- c()
    cats <- filtered[[item]]
    i <- 1
    for(cat in cats) {
      for(cat2 in cats) {
        if(cat == cat2) {
          next
        }
        temp <- filtered[cats == cat,][c("n", "denom")] %>%
          rbind(filtered[cats == cat2,][c("n", "denom")]) %>%
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
        names(p.value) <- glue::glue("{cat} & {cat2} p-value:")
        
        if(any(
          glue::glue("{cat2} & {cat} p-value:") %in% names(p.values),
          p.value > conf,
          is.na(p.value))) {
          next
        }
        p.values <- c(p.values, p.value)
        #print(glue::glue("{i}: {cat}, {cat2}"))
        #i <- i+1
      }
    }
    
    # return plots that have at least on statistically significant value
    if(is.null(p.values) & is.null(show)) {
      return(NULL)
    } else {
      plot <- filtered %>% #filter(n >= min) %>%
        # plot
        ggplot(aes(x = prop, y = #stringr::str_to_title(labelled::to_character(!!sym_item))
                     reorder(stringr::str_to_title(labelled::to_character(!!sym_item)), prop))) +
        geom_col(fill = project_pal[4]) +
        geom_vline(xintercept = p, lty = "dashed", color = project_pal[1]) +
        #geom_col(aes(x = p, y = "All")) +
        
        # colors
        scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
        scale_color_discrete(guide = "legend", name = item) + 
        project_theme + # question: add a variable for this?
        
        # labels
        xlab(NULL) + ylab(NULL) +
        ggtitle(glue::glue("{stringr::str_to_title(title)}\nby {stringr::str_to_title(item)}")) +
        labs(subtitle = paste(names(p.values), p.values, collapse = "\n")) +
        geom_text(aes(label = glue::glue("{scales::percent(signif(prop, 4))}\n{n}/{denom}")),
                  color = project_pal[4], hjust = 0, nudge_x = 0.01) 
      
        if(min(reshaped$n) < 1) {
          pulled <- reshaped %>% filter(denom < 1) %>%
            mutate_if(labelled::is.labelled, labelled::to_character) %>%
            pull(!!sym_item)
          cats <- #glue::glue("'{pulled}'") %>%
            paste(pulled, collapse = ", ")
          plot <- plot +
            labs(caption = glue::glue("*Categories with too few responses excluded: '{cats}'"))
        }
      return(plot)
    }
  })
  return(out)
}
