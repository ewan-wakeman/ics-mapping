#theme.r

colours <- c(dark = '#222629',
             light = '#D7D2C8',
             storm = '#494E6B',
             cloud = '#98878F',
             sunset = '#985E6D',
             evening = '#192231',
             coral = '#E14658',
             fresh = '#F7Ef6A',
             grape = '#575DA9',
             fuschia = '#E42D9F',
             blue = '#0b53c1')

theme_light <-
  function(base_family = 'Helvetica Neue Thin', base_size = 10){
    theme_minimal(base_family = base_family, base_size = base_size) %+%
      theme(
        plot.background = element_rect(fill = colours[['light']], colour = 'transparent'),
        panel.grid = element_blank(),
        plot.title = element_text(size = rel(1.5), margin = margin(10,2,10,2)),
        text = element_text(colour = colours[['dark']])
      )
  }

theme_dark <-
  function(base_family = 'Helvetica Neue Thin', base_size = 10){
    theme_minimal(base_family = base_family, base_size = base_size) %+%
      theme(
        plot.background = element_rect(fill = colours[['dark']], colour = 'transparent'),
        panel.grid = element_blank(),
        plot.title = element_text(size = rel(1.5), margin = margin(10,2,10,2)),
        text = element_text(colour = colours[['light']]),
        axis.text = element_text(colour = colours[['light']])
      )
  }