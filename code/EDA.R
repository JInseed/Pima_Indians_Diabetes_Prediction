install.packages('ggcorrplot')
library(ggcorrplot)

#상관관계


corrplot::corrplot(cor(df %>% 
  na.omit() %>% 
    select(-Outcome)
))


ggcorrplot(cor(df %>% 
                 na.omit() %>% 
                 select(-Outcome)), 
           hc.order = F, 
           type = "upper",
           lab = T)

ggcorrp
#Insulin 과 Glucose
#BMI 와 SkinThickness
#Age 와 Pregnancies

colnames(df)
#변수별 당뇨병 비율
df %>% 
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















