#변수별 당뇨병 유무 평균
mu=df %>% 
  na.omit() %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  group_by(Outcome) %>% 
  summarise(Pregnancies=mean(Pregnancies),
            Glucose=mean(Glucose),
            BloodPressure=mean(BloodPressure),
            SkinThickness=mean(SkinThickness),
            Insulin=mean(Insulin),
            BMI=mean(BMI),
            DiabetesPedigreeFunction=mean(DiabetesPedigreeFunction),
            Age=mean(Age))

mu

#결측치 시각화
table(apply(is.na(df),1,sum))

miss_var_summary(df)

df %>% 
  arrange(Insulin, SkinThickness, BloodPressure, BMI) %>% 
  vis_miss()

aggr(df, col=c('white','red'),prop=F, number=T, sortVars=T,
     gap=1, cex.axis=.5)




#결측 존재하는 열 분포시각화

##Glucose
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome))+
  geom_histogram(aes(Glucose),
                 col='white',
                 binwidth = 5,
                 position = 'fill',
                 alpha=0.7)+
  ylab('비율')

#겹쳐그리기
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, Glucose))+
  geom_histogram(col='white',
                 binwidth = 5,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=5.5*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=Glucose, color=Outcome),
             linetype="dashed", size=1)+
  ylab('Count')


df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, Glucose))+
  geom_histogram(col='white',
                 binwidth = 5,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=5.5*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=Glucose, color=Outcome),
             linetype="dashed", size=1)+
  facet_grid(Outcome ~ .)+
  ylab('Count')


df %>%
  filter(is.na(Glucose)) %>% 
  mutate(Outcome=as.factor(Outcome),
         Glucose=as.factor(Glucose)) %>%
  group_by(Glucose,Outcome) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n))


##BloodPressure
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome))+
  geom_histogram(aes(BloodPressure),
                 col='white',
                 binwidth = 5,
                 alpha=.7,
                 position = 'fill')+
  ylab('비율')

#겹쳐그리기
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, BloodPressure))+
  geom_histogram(col='white',
                 binwidth = 5,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=5.5*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=BloodPressure, color=Outcome),
             linetype="dashed", size=1)+
  ylab('Count')


df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, BloodPressure))+
  geom_histogram(col='white',
                 binwidth = 5,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=5.5*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=BloodPressure, color=Outcome),
             linetype="dashed", size=1)+
  facet_grid(Outcome ~ .)+
  ylab('Count')



df %>%
  filter(is.na(BloodPressure)) %>% 
  mutate(Outcome=as.factor(Outcome),
         BloodPressure=as.factor(BloodPressure)) %>%
  group_by(BloodPressure,Outcome) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n))


#SkinThickness
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome))+
  geom_histogram(aes(SkinThickness),
                 col='white',
                 binwidth = 5,
                 position = 'fill',
                 alpha=0.7)+
  ylab('비율')

#겹쳐그리기
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, SkinThickness))+
  geom_histogram(col='white',
                 binwidth = 5,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=5.5*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=SkinThickness, color=Outcome),
             linetype="dashed", size=1)+
  ylab('Count')


df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, SkinThickness))+
  geom_histogram(col='white',
                 binwidth = 5,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=5.5*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=SkinThickness, color=Outcome),
             linetype="dashed", size=1)+
  facet_grid(Outcome ~ .)+
  ylab('Count')


df %>%
  filter(is.na(SkinThickness)) %>% 
  mutate(Outcome=as.factor(Outcome),
         SkinThickness=as.factor(SkinThickness)) %>%
  group_by(SkinThickness,Outcome) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n))



#Insulin
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome))+
  geom_histogram(aes(Insulin),
                 col='white',
                 binwidth = 10,
                 position = 'fill',
                 alpha=0.7)

#겹쳐그리기
df %>% 
  filter(Insulin < 400) %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, Insulin))+
  geom_histogram(col='white',
                 binwidth = 10,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=11*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=Insulin, color=Outcome),
             linetype="dashed", size=1)+
  ylab('Count')


df %>% 
  filter(Insulin < 400) %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, Insulin))+
  geom_histogram(col='white',
                 binwidth = 10,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=11*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=Insulin, color=Outcome),
             linetype="dashed", size=1)+
  facet_grid(Outcome ~ .)+
  ylab('Count')




df %>%
  filter(is.na(Insulin)) %>% 
  mutate(Outcome=as.factor(Outcome),
         Insulin=as.factor(Insulin)) %>%
  group_by(Insulin,Outcome) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n))



#BMI
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome))+
  geom_histogram(aes(BMI),
                 col='white',
                 binwidth=2,
                 position = 'fill',
                 alpha=.7)+
  ylab('비율')

#겹쳐그리기
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, BMI))+
  geom_histogram(col='white',
                 binwidth = 2,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=2.2*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=BMI, color=Outcome),
             linetype="dashed", size=1)+
  ylab('Count')


df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, BMI))+
  geom_histogram(col='white',
                 binwidth = 2,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=2.2*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=BMI, color=Outcome),
             linetype="dashed", size=1)+
  facet_grid(Outcome ~ .)+
  ylab('Count')




df %>%
  filter(is.na(BMI)) %>% 
  mutate(Outcome=as.factor(Outcome),
         BMI=as.factor(BMI)) %>%
  group_by(BMI,Outcome) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n))


#나머지 열 분포시각화
#Pregnancies
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome))+
  geom_histogram(aes(Pregnancies),
                 col='white',
                 binwidth = 1,
                 position = 'fill',
                 alpha = .7)+
  ylab('비율')

#겹쳐그리기
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, Pregnancies))+
  geom_histogram(col='white',
                 binwidth = 1,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=1.2*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=Pregnancies, color=Outcome),
             linetype="dashed", size=1)+
  ylab('Count')


df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, Pregnancies))+
  geom_histogram(col='white',
                 binwidth = 1,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=1.2*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=Pregnancies, color=Outcome),
             linetype="dashed", size=1)+
  facet_grid(Outcome ~ .)+
  ylab('Count')




#DiabetesPedigreeFunction
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome))+
  geom_histogram(aes(DiabetesPedigreeFunction),
                 col='white',
                 binwidth = .1,
                 position = 'fill',
                 alpha=.7)+
  ylab('비율')

#겹쳐그리기
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, DiabetesPedigreeFunction))+
  geom_histogram(col='white',
                 binwidth = .1,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=.1*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=DiabetesPedigreeFunction, color=Outcome),
             linetype="dashed", size=1)+
  ylab('Count')


df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, DiabetesPedigreeFunction))+
  geom_histogram(col='white',
                 binwidth = .1,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=.1*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=DiabetesPedigreeFunction, color=Outcome),
             linetype="dashed", size=1)+
  facet_grid(Outcome ~ .)+
  ylab('Count')



#Age
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome))+
  geom_histogram(aes(Age),
                 col='white',
                 binwidth = 2,
                 position = 'fill',
                 alpha = .7)+
  ylab('비율')


df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, Age))+
  geom_histogram(col='white',
                 binwidth = 2,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=3*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=Age, color=Outcome),
             linetype="dashed", size=1)+
  ylab('Count')


df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(fill=Outcome, Age))+
  geom_histogram(col='white',
                 binwidth = 2,
                 position = 'identity',
                 alpha=0.5)+
  geom_density(aes(y=3*..count.., col=Outcome),
               size=0.7,
               alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=Age, color=Outcome),
             linetype="dashed", size=1)+
  facet_grid(Outcome ~ .)+
  ylab('Count')


#상관관계 큰 변수 간 시각화
#Insulin 과 Glucose
#BMI 와 SkinThickness
#Age 와 Pregnancies

#Insulin, Glucose
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(Insulin, Glucose,col=Outcome))+
  geom_point(cex=4,
             alpha=.3)+
  geom_smooth(se = T,
              alpha=.2)

df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(Insulin, Glucose,col=Outcome))+
  geom_point(cex=4,
             alpha=.3)+
  geom_smooth(se = T,
              alpha=.2)+
  facet_grid(Outcome ~ .)



#BMI, SkinThickness
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(BMI, SkinThickness,col=Outcome))+
  geom_point(cex=4,
             alpha=.3)+
  geom_smooth(se = T,
              alpha=.2)

df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(BMI, SkinThickness,col=Outcome))+
  geom_point(cex=4,
             alpha=.3)+
  geom_smooth(se = T,
              alpha=.2)+
  facet_grid(Outcome ~ .)

#Age, Pregnancies
df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(Age, Pregnancies,col=Outcome))+
  geom_point(cex=4,
             alpha=.3)+
  geom_smooth(se = T,
              alpha=.2)

df %>% 
  mutate(Outcome=as.factor(Outcome)) %>%
  ggplot(aes(Age, Pregnancies,col=Outcome))+
  geom_point(cex=4,
             alpha=.3)+
  geom_smooth(se = T,
              alpha=.2)+
  facet_grid(Outcome ~ .)







