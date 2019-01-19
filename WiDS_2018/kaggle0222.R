
train1 <- read.csv("/Users/skang/Dropbox/WiDS 2018 Kaggle/dataFormodel1_0206/rawdata/train.csv")
test1 <- read.csv("/Users/skang/Dropbox/WiDS 2018 Kaggle/dataFormodel1_0206/rawdata/test.csv")

library(dplyr)
f1 <- function(train1){
  #First level variables
  name1 <- c("DG4","DG6","DL0","DL1","DL3","DL6","DL7","MT1A","MT2","MT3_1",
             paste0("MT4_",1:6),"MT5","MT6","MT7","MT8","MT10","MT14C_1","MT15",
             "MT17_1","MT17_2","MT18_1","FF1","FF2","LN1B","GN1","GN2","GN3","GN4","GN5",
             "FL12","DG3","FF13","LN1A","FL4","FF2A","AA4","AA14","AA15","DG1","DL5")
  
  ##second level variables
  name2 <- c("AA3","AA5","AA6","DG3A",paste0("DG5_",1:11),"DG8a","DG8b","DG8c",
             "DG9a","DG9b","DG9c","DG10b","DG10c","DG11b","DG11c","DG12B_1","DG12B_2","DG12C_1","DG12C_2","DG13_2",
             "DG13_7","DG14","DL4_1","DL4_5","DL4_6","DL4_16","DL12","DL13",paste0("DL",15:23),
             "DL24",paste0("DL25_",1:8),"DL26_12","DL26_1","DL27","G2P1_1","G2P1_11",
             "MT1","MT3_2","MT3_3","MT6A","MT6B","MT7A","MT9",paste0("MT12_",c(1,2,7,11)),
             "MT14A_2","MT14B","MT16_1","MT16_3","MT17_3","MT17_4",paste0("MT18_",c(2:6,8)),
             paste0("MT18A_",1:4),"FF4","FF5",paste0("FF6_",1:10),"FF7_1","FF7_4",
             "FF9",paste0("FF10_",1:3),"FF13",paste0("FF14_",c(1,2,3,23)),paste0("FF16_",1:3),
             "FF19_1","FF19_4","MM1","MM2_16","MM3_2","MM4_2","MM6_13","IFI1_1","IFI1_3",
             "IFI1_5","IFI1_7","IFI12_20","IFI14_1","IFI14_2","IFI15_1","IFI15_2","IFI16_1","IFI16_2",
             "IFI16_3","IFI16_4","IFI18","IFI20_1","IFI20_2","IFI22_1","IFI22_9","IFI24","FL1",
             "FL2",paste0("FL6_",1:4),"FL7_1",paste0("FL8_",1:3),paste0("FL9",c("A","B","C")),"FL10","FL11",
             paste0("FL",13:18),paste0("FB1_",1:3),"FB2","FB3","FB4_1","FB14","FB16_8","FB16A_8",
             "FB17A_6","FB18","FB19A_1","FB19B_1","FB20","FB21","FB22_1","FB22_8","FB23_1","FB23_9",
             "FB24","FB26_1","FB26_8","FB27_2","FB29_1","LN1B",paste0("LN2_",1:4))
  
  names <- colnames(train1) 
  idx <- which(names %in% c(name1,name2))
  dat1 <- train1[,idx]
  dat1[is.na(dat1)] <- 0
  dat2 <- dat1
  
  dat2$FB14 <- case_when(dat1$FB14 ==0 ~1,
                          (dat1$FB14 >0)~2)
  dat2$FB21 <- case_when(dat1$FB21 ==0 ~1,
                         (dat1$FB21 >0)~2)
  dat2$FB24 <- case_when(dat1$FB24 ==0 ~0,
                         (dat1$FB21 ==1)~1,
                         (dat1$FB21 > 1 & dat1$FB21 <15 )~2,
                         dat1$FB21 >=15 ~3)
  
  dat2$MM3_2 <- case_when(dat1$MM3_2 ==2 ~1,
                          (dat1$MM3_2 <2)~2)
  dat2$MM4_2 <- case_when(dat1$MM4_2 ==0 ~1,
                          (dat1$MM4_2 >0)~2)
  dat2$MM6_13 <- case_when(dat1$MM6_13 ==0 ~1,
                          (dat1$MM6_13 >0)~2)
  dat2$IFI12_20 <- case_when(dat1$IFI12_20 ==0 ~1,
                           (dat1$IFI12_20 >0)~2)
  dat2$IFI16_1 <- case_when(dat1$IFI16_1 ==0 ~0,
                             (dat1$IFI16_1 ==1)~1,
                            dat1$IFI16_1 ==2 ~2,
                            (dat1$IFI16_1 ==3)~3,
                            dat1$IFI16_1 %in% c(4,5) ~4,
                            (dat1$IFI16_1 >8)~8)
  dat2$IFI16_2 <- case_when(dat1$IFI16_2 ==0 ~0,
                            (dat1$IFI16_2 ==1)~1,
                            dat1$IFI16_2 ==2 ~2,
                            (dat1$IFI16_2 ==3)~3,
                            dat1$IFI16_2 %in% c(4,5) ~4,
                            (dat1$IFI16_2 >8)~8)
  
  dat2$IFI16_3 <- case_when(dat1$IFI16_3 ==0 ~1,
                          (dat1$IFI16_3 >0)~2)
  dat2$IFI16_4 <- case_when(dat1$IFI16_4 ==0 ~1,
                          (dat1$IFI16_4 >0)~2)
  dat2$IFI18 <- case_when(dat1$IFI18 ==0 ~0,
                            (dat1$IFI18==1)~1,
                          (dat1$IFI18>1)~2)
  
  dat2$IFI22_1 <- case_when(dat1$IFI22_1 ==0 ~0,
                          (dat1$IFI22_1>=1)~1)
  
  dat2$FF7_1 <- case_when(dat1$FF7_1 ==0 ~1,
                         (dat1$FF7_1 >0)~2)
  
  dat2$FF7_4 <- case_when(dat1$FF7_4 ==0 ~1,
                          (dat1$FF7_4 >0)~2)
  dat2$FF13 <- case_when(dat1$FF13 ==1 ~1,
                         (dat1$FF13 == 2)~2,
                         (dat1$FF13 == 0)~0,
                         dat1$FF13 >=3~3)
  dat2$FF16_1 <- case_when(dat1$FF16_1 ==0 ~0,
                          (dat1$FF16_1 %in% c(1,2,3))~1,
                          (dat1$FF16_1 %in% c(4,5))~2)
  
  dat2$FF16_2 <- case_when(dat1$FF16_2 ==0 ~0,
                           (dat1$FF16_2 %in% c(1,2,3))~1,
                           (dat1$FF16_2 %in% c(4,5))~2)
  dat2$FF16_3 <- case_when(dat1$FF16_3 ==0 ~0,
                           (dat1$FF16_3 >0)~1)
  
  dat2$DG8a <- case_when(dat1$DG8a ==1 ~1,
                         (dat1$DG8a == 2)~2,
                         dat1$DG8a ==3~3,
                         dat1$DG8a ==4~4,
                         (dat1$DG8a > 5 & dat1$DG8a < 90)~5,
                         (dat1$DG8a == 0 & dat1$DG8a > 90)~6)
  
  dat2$DG8b <- case_when(dat1$DG8b ==0 ~0,
                         (dat1$DG8b == 1)~1,
                         dat1$DG8b ==2~2,
                         dat1$DG8b >2~3)
  dat2$DG8c <- case_when(dat1$DG8c ==0 ~0,
                         (dat1$DG8c == 1)~1,
                         dat1$DG8c ==2~2,
                         dat1$DG8c >2~3)
  dat2$DG9a <- case_when(dat1$DG9a ==0 ~0,
                         (dat1$DG9a == 1)~1,
                         dat1$DG9a ==2~2,
                         dat1$DG9a >2~3)
  dat2$DG9b <- case_when(dat1$DG9b ==0 ~0,
                         (dat1$DG9b >= 1)~1)
  dat2$DG9c <- case_when(dat1$DG9c ==0 ~0,
                         (dat1$DG9c >= 1)~1)
  
  dat2$DG10b <- case_when(dat1$DG10b ==0 ~0,
                         (dat1$DG10b == 1)~1,
                         (dat1$DG10b > 1)~2)
  dat2$DG10c <- case_when(dat1$DG10c ==0 ~0,
                          (dat1$DG10c == 1)~1,
                          (dat1$DG10c > 1)~2)
  dat2$DG11b <- case_when(dat1$DG11b ==0 ~0,
                          (dat1$DG11b == 1)~1,
                          (dat1$DG11b > 1)~2)
  dat2$DG11c <- case_when(dat1$DG11c ==0 ~0,
                          (dat1$DG11c == 1)~1,
                          (dat1$DG11c > 1)~2)
  dat2$DG14 <- case_when(dat1$DG14 ==0 ~1,
                         (dat1$DG14 != 0)~2)
  dat2$DL12 <- case_when(dat1$DL12 ==0 ~1,
                         (dat1$DL12 != 0)~2)
  dat2$DL13 <- case_when(dat1$DL13 ==0 ~1,
                         (dat1$DL13 != 0)~2)
  dat2$DL24 <- case_when(dat1$DL24 <=3 ~1,
                         (dat1$DL24 >= 4)~2)
  dat2$DL27 <- case_when(dat1$DL27 <=0 ~1,
                         (dat1$DL27 >= 1)~2)
  dat2$MT1 <- case_when(dat1$MT1 ==0 ~0,
                        (dat1$MT1 == 1)~1,
                        dat1$MT1 ==2~2,
                        dat1$MT1 >2~3)
  
  dat2$MT7A <- case_when(dat1$MT7A ==0 ~0,
                        (dat1$MT7A == 1)~1,
                        dat1$MT7A >=2~2)
  dat2$MT9 <- case_when(dat1$MT9 ==0 ~0,
                         (dat1$MT9 >= 1)~1)
  
  dat2$MT12_1 <- case_when(dat1$MT12_1 ==0 ~0,
                        (dat1$MT12_1 >= 1)~1)
  dat2$MT12_2 <- case_when(dat1$MT12_2 ==0 ~0,
                           (dat1$MT12_2 >= 1)~1)
  dat2$MT12_7 <- ifelse(dat1$MT12_7==0,0,1)
  dat2$MT12_11 <- ifelse(dat1$MT12_11==0,0,1)
  dat2$MT14A_2 <- ifelse(dat1$MT14A_2==0,0,1)
  dat2$MT14B <- ifelse(dat1$MT14B==0,0,1)
  
  
  dat2$DG3A <- case_when(dat1$DG3A ==4 ~1,
                         (dat1$DG3A != 4)~2)
  dat2$AA5 <- case_when(dat1$AA5 ==0 ~1,
                         (dat1$AA5> 0)~2)
  
  
  dat2$DG1 <- case_when(dat1$DG1 <= 1975 ~1,
                         (dat1$DG1 > 1975)~2)
  
  dat2$AA14 <- case_when(dat1$AA14 <= 1700 ~1,(dat1$AA14> 1700 & dat1$AA14 <=3100) ~2,
                         (dat1$AA14 > 3100 & dat1$AA14 <= 4800)~3,
                         (dat1$AA14 > 4800)~4)
  
  dat2$AA15 <- case_when(dat1$AA15 <= 155 ~1,(dat1$AA15> 155 & dat1$AA15 <=270) ~2,
                         (dat1$AA15 > 270 & dat1$AA15 <= 400)~3,
                         (dat1$AA15 > 400 & dat1$AA15 <= 560)~4,
                         dat1$AA15 > 560 ~ 5)

  dat2$FL4 <- case_when(dat1$FL4 ==1~1,(dat1$FL4 == 2) ~2,
                        (dat1$FL4 >=3 & dat1$FL4 <= 7)~3,
                        (dat1$FL4 ==8)~4,
                        (dat1$FL4 > 8)~5)
  dat2$FL12 <- case_when(dat1$FL12 ==1~1,
                        (dat1$FL12 >= 2) ~2)
  dat2$FF13 <- case_when(dat1$FF13 ==0~0,dat1$FF13 ==1~1,dat1$FF13 ==2~2,
                         (dat1$FF13 >= 3) ~3)
  
  #dat2$DG3 <- case_when(dat1$DG3 ==1~1,
                     #   (dat1$DG3 >= 2 & dat1$DG3 <= 5) ~2,
                     #   dat1$DG3 >= 6 ~ 3)
  
  dat2$DG4 <- case_when(dat1$DG4 ==1~1,
                        (dat1$DG4 >= 2 & dat1$DG4 <= 5) ~2,
                        dat1$DG4 >= 6 ~ 3)
  dat2$DG6 <- case_when(dat1$DG6 ==1~1,
                        (dat1$DG6 == 2 ) ~2,
                        dat1$DG6 >= 3 ~ 3)
  dat2$DL1 <- case_when(dat1$DL1 ==7~1,
                        (dat1$DL1 != 7 ) ~2)
  
  dat2$MT1A <- case_when(dat1$MT1A ==1~1,
                         (dat1$MT1A == 2 ) ~2,
                         (dat1$MT1A==0 |  dat1$MT1A >=3) ~ 3)
  dat2$MT3_1 <- case_when(dat1$MT3_1 ==0~0,
                          (dat1$MT3_1>=1) ~1)
  dat2$MT3_2 <- case_when(dat1$MT3_2 ==0~0,
                          (dat1$MT3_2>=1) ~1)
  dat2$MT3_3 <- case_when(dat1$MT3_3 ==0~0,
                          (dat1$MT3_3>=1) ~1)
  dat2$MT5 <- case_when(dat1$MT5 ==0~0,
                        (dat1$MT5>=1) ~1)
  dat2$MT6 <- case_when(dat1$MT6 ==0~0,
                        (dat1$MT6 ==1) ~1,
                        (dat1$MT6 ==2) ~2,
                        (dat1$MT6 >2) ~3)
  dat2$MT6A <- case_when(dat1$MT6A ==0~0,
                         (dat1$MT6A ==1) ~1,
                         (dat1$MT6A >=2) ~2)
  dat2$MT6B <- case_when(dat1$MT6B ==0~0,
                         (dat1$MT6B ==1) ~1,
                         (dat1$MT6B >=2) ~2)
  
  dat2$MT8 <- case_when(dat1$MT8 ==0~0,
                        (dat1$MT8 >=1) ~1)
  dat2$MT17_1 <- case_when(dat1$MT17_1 ==0~0,
                           (dat1$MT17_1 ==1) ~1,
                           (dat1$MT17_1 > 1) ~2)
  dat2$MT14C_1 <- case_when(dat1$MT14C_1 ==0~0,
                            (dat1$MT14C_1 >=1) ~1)
  dat2$GN1 <- case_when(dat1$GN1 ==0~0,
                        (dat1$GN1 ==1) ~1,
                        (dat1$GN1 > 1) ~2)
  
  dat2$GN2 <- case_when((dat1$GN2 ==1) ~1,
                        (dat1$GN2 ==2) ~2,
                        (dat1$GN2 ==3) ~3,
                        (dat1$GN2 > 3) ~4)
  dat2$GN3 <- case_when((dat1$GN3 ==1) ~1,
                        (dat1$GN3 ==2) ~2,
                        (dat1$GN3 ==3) ~3,
                        (dat1$GN3 > 3) ~4)
  dat2$GN4 <- case_when((dat1$GN4 ==1) ~1,
                        (dat1$GN4 ==2) ~2,
                        (dat1$GN4 ==3) ~3,
                        (dat1$GN4 > 3) ~4)
  dat2$GN5 <- case_when((dat1$GN5 ==1) ~1,
                        (dat1$GN5 ==2) ~2,
                        (dat1$GN5 ==3) ~3,
                        (dat1$GN5 > 3) ~4)
  return(dat2)
}

train2 <- f1(train1)
train2$is_female <- train1$is_female
test2 <- f1(test1)

