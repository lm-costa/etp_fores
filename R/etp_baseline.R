### fun degree to rad
deg2rad <- function(x){
  return((x*pi)/180)
}

### CFSR

tab <- read.csv('data-raw/database.csv')




tab <- tab |>
  dplyr::mutate(year= lubridate::year(dates),
                month = lubridate::month(dates)) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(prec=sum(precipitation),
                   temp=mean(tm)) |>
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

tab_final <- data.frame(
  lon = tab$longitude,
  lat=tab$latitude,
  year=tab$year,
  month=tab$month,
  prec=tab$prec,
  temp=tab$temp,
  etp= tab$ETP)

write.csv(tab_final,'data/et_cfsr.csv')




## NASA POWER

file_n <- list.files('data-raw/power/',pattern = 'csv')
for (i in 1:length(file_n)){
  if(i ==1){
    df <- read.csv(paste0('data-raw/power/',file_n[i]), sep=',')
  }else{
    df_a <- read.csv(paste0('data-raw/power/',file_n[i]), sep=',')
    df <- rbind(df,df_a)
  }
}


tab_power <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
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


tab_final_power <- data.frame(
  lon = tab_power$longitude,
  lat=tab_power$latitude,
  year=tab_power$year,
  month=tab_power$month,
  prec=tab_power$prec,
  temp=tab_power$temp,
  etp= tab_power$ETP)

write.csv(tab_final_power,'data/et_power.csv')

