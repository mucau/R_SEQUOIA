# Lancement des library
if (!require("ClimClass")) {install.packages("ClimClass")}
if (!require("geosphere")) {install.packages("geosphere")}

THORNTHWAITE <- function(series,  latitude, ru=80,
         snow.init=0, Tsnow=2, fr.sn.acc=0.95,  snow_melt_coeff=1) {
  # Variables annuelles
  month_lengths<-c(31,28,31,30,31,30,31,31,30,31,30,31)

  # indice thermique mensuel
  yDay <- strptime(paste(15,"/",1:12,"/",2014,sep=""), format="%d/%m/%Y")$yday + 1
  rel_daylength <- geosphere::daylength(latitude,yDay)/12

  # calculates extra-atmospheric radiation for each day in the set
  chg <- ClimClass::ExAtRa(DOY=yDay,latitude=latitude, unit="mm")

  # Calcul de la RU mobilisable
  TAW <- ru
  fTAW<-(-0.000000105*TAW^2+0.000054225*TAW-0.00878)
  ultStorPrA <- TAW

  # D???termination de la p???riode de calcul
  first.yr<-min(series$year)
  last.yr<-max(series$year)
  years <- first.yr:last.yr

  # Cr???ation du data frame
  snowpack_jan <- snow.init
  tab <- data.frame()

  for(ii in 1:length(years)) {
    zzz<-data.frame(series[series$year==years[ii],])
    df <- zzz[,1:2]
    zzz<-data.frame(zzz$P, (zzz$Tn + zzz$Tx)/2, zzz$Tn, zzz$Tx); names(zzz)<-c("Prec1", "Tmean1","Tn1", "Tx1")

    # check on residual missing values
    if(sum(is.na(zzz$Prec1))!=0 | sum(is.na(zzz$Tmean1))!=0) {
      cat(paste0("Monthly values missing in year", first.yr+ii-1, "\n"))
    } else {
      Prec  <-zzz[,1]
      Tmean <-zzz[,2]
      Tn    <-zzz[,3]
      Tx    <-zzz[,4]

      Storage<-rep(TAW,length(Tmean))
      Percola<-rep(0,length(Tmean))

      # Calcul de l'indice thermique
      TmeanCor<-Tmean
      TmeanCor[TmeanCor<0]<-0
      ITM <- ((TmeanCor/5)^1.514) # monthly
      ITA <- sum(ITM) # annual

      # Calcul de l'ETP
      exp<- 0.000000675*ITA^3 - 0.0000771 *ITA^2 + 0.01792*ITA + 0.492
      PET<-(16*(10*TmeanCor/ITA)^exp)*rel_daylength

      # Accumulation de neige
      SnowAcc<-rep(0,12)
      SnowPrec<-Prec; SnowPrec[Tmean>=Tsnow]<-0

      # calculates maximum snowpack before first thaw month
      month_max<-max(min(which(Tmean>=Tsnow)-1), 1) # month before first with Tmean >= Tsnow
      SnowAcc_wint<-NULL

      for(i in 1:month_max) {
        if(i==1){
          SnowAcc_wint[i] <- snowpack_jan + SnowPrec[i]*fr.sn.acc
        } else {
          SnowAcc_wint[i]<-SnowAcc_wint[i-1] + SnowPrec[i]*fr.sn.acc
        }
      }

      snowpack_ref<-SnowAcc_wint[month_max]
      snow_depl<-NULL
      SnowAcc[1] <- SnowAcc_wint[1]
      snow_depl[1] <- SnowPrec[1]*(1-fr.sn.acc) # 0 if Tmean>=Tsnow

      if(Tmean[1]>=Tsnow) {
        snow_depl[1] <- snow_melt_coeff[1]*SnowAcc[1]
        SnowAcc[1]<- SnowAcc[1]-snow_depl[1]
      }

      # following months
      count_thaw<-0
      for(i in 2:12) {
        snow_depl[i]<-  SnowPrec[i]*(1-fr.sn.acc) # 0 if Tmean>=Tsnow
        SnowAcc[i]<- SnowAcc[i-1]+SnowPrec[i]*fr.sn.acc
        if(Tmean[i]>=Tsnow) {
          count_thaw<-count_thaw+1
          if(count_thaw>length(snow_melt_coeff)) {
            snow_depl[i]<- SnowAcc[i]
          } else {
            snow_depl[i]<- snow_depl[i] + SnowAcc[i] *snow_melt_coeff[count_thaw]
          }
        }
        SnowAcc[i]<- SnowAcc[i]-snow_depl[i]
      }
      snowpack_jan<-SnowAcc[12]

      # Calcul de la diff???rence entre Prec and PET
      Liq_Prec<- Prec; Liq_Prec[Tmean<Tsnow] <- Prec[Tmean<Tsnow]*(1-fr.sn.acc)
      P.minus.ET<- Liq_Prec + snow_depl - PET

      # Balance
      if(ii==1)   # first year, field capacity
        last_Storage<-TAW
      # first month
      if(!is.na(P.minus.ET[1]) & P.minus.ET[1]>0) {
        Storage[1]<-last_Storage+P.minus.ET[1]
        if(Storage[1]>TAW) {
          Percola[1]<-Storage[1]-TAW
          Storage[1]<-TAW
        }
      }  else {
        PETvir<-(log10(TAW)-log10(last_Storage))/fTAW # virtual PET
        Storage[1]<-TAW*10^(-(PETvir + P.minus.ET[1])*fTAW)
      }

      # following months
      for(i in 2:length(Storage)) {
        if(!is.na(P.minus.ET[i]) & P.minus.ET[i]>0) {
          Storage[i]<-Storage[i-1]+P.minus.ET[i]
          if(Storage[i]>TAW) {
            Percola[i]<-Storage[i]-TAW
            Storage[i]<-TAW
          }
        } else {
          PETvir<-(log10(TAW)-log10(Storage[i-1]))/fTAW
          Storage[i]<-TAW*10^(-(PETvir+P.minus.ET[i])*fTAW)
        }
      }

      # saves the last storage
      last_Storage<-Storage[12]
      # calculates real ET as difference between prec. and storage
      delta.sto<-c(0,diff(Storage))
      ETr<-(Liq_Prec + snow_depl - delta.sto)
      for(i in 1:length(ETr)) {
        if(P.minus.ET[i]>0) {ETr[i]<-PET[i]}
      }
      # deficit: difference between real and potential ET (non-negative only)
      Def <- PET - ETr; Def[Def<0]<-0

      # water surplus
      Sur <- Liq_Prec + snow_depl - ETr - (TAW - Storage)
      Sur[Sur<0]<-0

      df <- cbind(df, Prec, PET, Storage, P.minus.ET, Def, Sur)
    } # end else (no missing monthly data)
    tab <- rbind(tab, df)
  } # end cycle on years

  names(tab) <- c("Annee","Mois", "Precipitation", "ETP", "RU", "P-ETP", "Deficit", "Surplus")
  tab <- tab %>%
    mutate(`P-ETP`= Precipitation-ETP,
           Surplus = case_when(`P-ETP`>=0 ~ `P-ETP`,
                               `P-ETP`<0 ~ 0))
  return(tab)
}
