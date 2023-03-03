file_names <- list.files('data-raw/cfsr_new')

deg2rad <- function(x){
  return((x*pi)/180)
}


for (i in 1:length(file_names)){
  if (i == 1){
    station <- substr(file_names[i],14,20)
    tab <- read.csv(paste0('data-raw/cfsr_new/',file_names[i]))
    tab['station'] <- station
  }else{
    station <- substr(file_names[i],14,20)
    tab_au <- read.csv(paste0('data-raw/cfsr_new/',file_names[i]))
    tab_au['station'] <- station
    tab <- rbind(tab,tab_au)
  }
}
rm(tab_au)

tab <- tab |>
  dplyr::mutate(year= lubridate::year(dates),
                month = lubridate::month(dates)) |>
  dplyr::group_by(station, longitude,latitude, year, month) |>
  dplyr::summarise(prec=sum(precipitation),
                   temp=mean(tm))


tab <- tab |>
  dplyr::group_by(station,longitude,latitude,month) |>
  dplyr::summarise(
    prec=mean(prec),
    temp=mean(temp)
  )

tab <- tab |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )



sto <- c()
nac <- c()

for(i in 1:nrow(tab)){
  month <- tab$month
  prec <- tab$prec
  ETP <- tab$ETP
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
  ETP <- tab$ETP
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



tab_f <- data.frame(
  station=tab$station,
  month=tab$month,
  prec=tab$prec,
  temp=tab$temp,
  etp= tab$ETP,
  etr=etr,
  def=def,
  exc=sur
)


#####
tab_f |>
  dplyr::mutate(
    ano = 2000,
    dia=1,
    data = lubridate::as_date(stringr::str_c(ano,month,dia, sep='-'))
    )|>
  ggplot2::ggplot(ggplot2::aes(x=lubridate::month(data),
                               y=prec, col=station))+
  ggplot2::geom_line()+
  ggplot2::scale_x_continuous(breaks = seq(0,12,by=2))+
  ggplot2::ylab(expression('Prec (mm '~month^-1~')'))+
  ggplot2::xlab('Month')+
  ggplot2::theme_classic()

ggplot2::ggsave('figures/prec_cl.png',units="in", width=10, height=5,
                dpi=300)


tab_f|>
  dplyr::mutate(
    ano = 2000,
    dia=1,
    data = lubridate::as_date(stringr::str_c(ano,month,dia, sep='-'))
  )|>
  ggplot2::ggplot(ggplot2::aes(x=lubridate::month(data),
                               y=temp, col=station))+
  ggplot2::geom_line()+
  ggplot2::geom_line()+
  ggplot2::scale_x_continuous(breaks = seq(0,12,by=2))+
  ggplot2::ylab(expression('Temp (ºC '~month^-1~')'))+
  ggplot2::xlab('Month')+
  ggplot2::theme_classic()

ggplot2::ggsave('figures/temp_cl.png',units="in", width=10, height=5,
                dpi=300)



tab_f |>
  dplyr::mutate(
    ano = 2000,
    dia=1,
    data = lubridate::as_date(stringr::str_c(ano,month,dia, sep='-'))
  )|>
  ggplot2::ggplot(ggplot2::aes(x=lubridate::month(data),
                               y=etp, col=station))+
  ggplot2::geom_line()+
  ggplot2::geom_line()+
  ggplot2::scale_x_continuous(breaks = seq(0,12,by=2))+
  ggplot2::ylab(expression('ETp (mm '~month^-1~')'))+
  ggplot2::xlab('Month')+
  ggplot2::theme_classic()

ggplot2::ggsave('figures/etp_cl.png',units="in", width=10, height=5,
                dpi=300)



tab_f |>
  dplyr::mutate(
    ano = 2000,
    dia=1,
    data = lubridate::as_date(stringr::str_c(ano,month,dia, sep='-'))
  )|>
  ggplot2::ggplot(ggplot2::aes(x=lubridate::month(data), y = exc))+
  ggplot2::geom_area(fill='darkblue')+
  ggplot2::geom_area(ggplot2::aes(y=-1*def), fill = 'red')+
  ggplot2::facet_wrap(~station,scales = 'free_y')+
  ggplot2::scale_x_continuous(breaks = seq(1,12,by=2))+
  ggplot2::scale_y_continuous(breaks = seq(-100,300,by=100))+
  ggplot2::ylab('mm/month')+
  ggplot2::xlab('months')


ggplot2::ggsave('figures/wb_est_cl.png',units="in", width=10, height=5,
                dpi=300)


write.csv(tab_f |>
            dplyr::mutate(
              wb = dplyr::case_when(
                exc > 0~ exc,
                exc <= 0 ~ -(def)
              ))
            ,'data/climatology.csv')
