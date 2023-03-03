tab <- read.csv('data/wd.csv')


### Water BAlance for observed data

sto <- c()
nac <- c()

for(i in 1:nrow(tab)){
  month <- tab$month
  prec <- tab$prec
  ETP <- tab$et_obs
  if(month[i]==1){
    petp <- prec-ETP
    if(petp[i]<0){
      nac[i] <- petp[i]
      sto[i] <- 100*(exp(1)^(nac[i]/100))
      if(sto[i]>100){
        sto[i] <- 100
      }else{
        sto[i] <- 100*(exp(1)^(nac[i]/100))
      }
    }else{
      sto[i] <- petp[i]
      if(sto[i]>100){
        sto[i] <- 100
      }else{
        sto[i] <- 100
      }
      nac[i] <- 100*log((sto[i]/100))
    }
  }else{
    petp <- prec-ETP
    if(petp[i]<0){
      nac[i] <- petp[i]+nac[i-1]
      sto[i] <- 100*(exp(1)^(nac[i]/100))
      if(sto[i]>100){
        sto[i] <- 100
      }else{
        sto[i] <- 100*(exp(1)^(nac[i]/100))
      }
    }else{
      sto[i] <- petp[i]+sto[i-1]
      if(sto[i]>100){
        sto[i] <- 100
      }else{
        sto[i] <- petp[i]+sto[i-1]
      }
      nac[i] <- 100*log((sto[i]/100))
    }
  }
}

alt <- c()
etr <- c()
sur <- c()
for(i in 1:nrow(tab)){
  month <- tab$month
  prec <- tab$prec
  ETP <- tab$et_obs
  if(month[i]==1){
    alt[i] <- 0
    if(alt[i]<0){
      etr[i] <- prec[i]+abs(alt[i])
    }else{
      etr[i] <- ETP[i]
    }
  }else{
    alt[i] <- sto[i]-sto[i-1]
    if(alt[i]<0){
      etr[i] <- prec[i]+abs(alt[i])
    }else{
      etr[i] <- ETP[i]
    }
  }

  def <- ETP-etr

  if(sto[i]<100){
    sur[i] <- 0
  }else{
    sur[i] <- petp[i]-alt[i]
  }

}



tab_obs <- data.frame(
  year=tab$year,
  month=tab$month,
  prec=tab$prec,
  etp_obs= tab$et_obs,
  etr_obs=etr,
  def_obs=def,
  exc_obs=sur
)

tab_obs <- tab_obs |>
  dplyr::mutate(
    bh_obs=dplyr::case_when(
      exc_obs > 0~ exc_obs,
      exc_obs <= 0 ~ -(def_obs)
    )
  )


####### Water Balance for estimated data


sto <- c()
nac <- c()

for(i in 1:nrow(tab)){
  month <- tab$month
  prec <- tab$prec
  ETP <- tab$et_est
  if(month[i]==1){
    petp <- prec-ETP
    if(petp[i]<0){
      nac[i] <- petp[i]
      sto[i] <- 100*(exp(1)^(nac[i]/100))
      if(sto[i]>100){
        sto[i] <- 100
      }else{
        sto[i] <- 100*(exp(1)^(nac[i]/100))
      }
    }else{
      sto[i] <- petp[i]
      if(sto[i]>100){
        sto[i] <- 100
      }else{
        sto[i] <- 100
      }
      nac[i] <- 100*log((sto[i]/100))
    }
  }else{
    petp <- prec-ETP
    if(petp[i]<0){
      nac[i] <- petp[i]+nac[i-1]
      sto[i] <- 100*(exp(1)^(nac[i]/100))
      if(sto[i]>100){
        sto[i] <- 100
      }else{
        sto[i] <- 100*(exp(1)^(nac[i]/100))
      }
    }else{
      sto[i] <- petp[i]+sto[i-1]
      if(sto[i]>100){
        sto[i] <- 100
      }else{
        sto[i] <- petp[i]+sto[i-1]
      }
      nac[i] <- 100*log((sto[i]/100))
    }
  }
}

alt <- c()
etr <- c()
sur <- c()
for(i in 1:nrow(tab)){
  month <- tab$month
  prec <- tab$prec
  ETP <- tab$et_est
  if(month[i]==1){
    alt[i] <- 0
    if(alt[i]<0){
      etr[i] <- prec[i]+abs(alt[i])
    }else{
      etr[i] <- ETP[i]
    }
  }else{
    alt[i] <- sto[i]-sto[i-1]
    if(alt[i]<0){
      etr[i] <- prec[i]+abs(alt[i])
    }else{
      etr[i] <- ETP[i]
    }
  }

  def <- ETP-etr

  if(sto[i]<100){
    sur[i] <- 0
  }else{
    sur[i] <- petp[i]-alt[i]
  }

}



tab_est <- data.frame(
  year=tab$year,
  month=tab$month,
  prec=tab$prec,
  etp_est= tab$et_est,
  etr_est=etr,
  def_est=def,
  exc_est=sur
)

tab_est <- tab_est |>
  dplyr::mutate(
    bh_est=dplyr::case_when(
      exc_est > 0~ exc_est,
      exc_est <= 0 ~ -(def_est)
    )
  )


###### Final table for compare


tab_f <- data.frame(
  year=tab$year,
  month=tab$month,
  wd_obs = tab_obs$bh_obs,
  wd_est = tab_est$bh_est
)

Metrics::rmse(tab_f$wd_obs,tab_f$wd_est)

tab_f |>
  dplyr::mutate(
    date = lubridate::make_date(year,month,'1'),
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(wd_obs=mean(wd_obs),
                   wd_est=mean(wd_est)) |>
  ggplot2::ggplot(ggplot2::aes(x=date))+
  ggplot2::geom_line(ggplot2::aes(y=wd_obs,col='WB obs'))+
  ggplot2::geom_line(ggplot2::aes(y=wd_est,col='WB est'))+
  ggplot2::geom_point(ggplot2::aes(y=wd_obs), col = 'red')+
  ggplot2::geom_point(ggplot2::aes(y=wd_est), col = 'darkblue')+
  ggplot2::ylab(expression('WB (mm '~month^-1~')'))+
  ggplot2::xlab('')+
  ggplot2::scale_color_manual(' ', values = c(`WB est` = 'darkblue',`WB obs`='red'))+
  ggplot2::theme(legend.position = 'right')

ggplot2::ggsave('figures/wb_est_vs_obs.png',units="in", width=10, height=5,
                dpi=300)


