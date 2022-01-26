USbirthsJan <- read.table("http://www.rossmanchance.com/iscam3/data/USbirthsJan2013.txt",header = T)
USbirthsJan1 <- USbirthsJan[which(USbirthsJan$fullterm ==1),] #เลือกเเถวเอาfulltermทุกคอลัมป์
hist(USbirthsJan1$birthweight, breaks = 20) #breakคืออาร์กิวเมน ที่ทำให้กราฟ(histogram:เเท่ง)เเสดงจำนวนกราฟเเท่งที่เรากำหนด(ความถี่ของเเท่ง)
w <- USbirthsJan1$birthweight[(USbirthsJan1$birthweight>=2000)&(USbirthsJan1$birthweight<=5000)] #เลือกข้อมู,น้ำหนัก2000-5000

hist(w,freq = F,xlab = "birth weight of the full term child (2000-5000 grams)",ylab = "probability", breaks = 25 )
lines(density(w),lwd=2,col="red") #การเเจกเเจงข้อมูลจากกราฟ
wfit <- seq(min(w),max(w))
ynorm <- dnorm(wfit,mean = mean(w),sd=sd(w)) #เพิ่มมาเพื่อสามารถหาค่า y ได้
lines(wfit,dnorm(wfit,mean = mean(w),sd=sd(w)),col="blue") #การเเจกเเจงข้อมูล"ปกติ"จากกราฟ

plot(density(w),lwd=2,col="red")

pnorm(3200,mean(w),sd(w))
