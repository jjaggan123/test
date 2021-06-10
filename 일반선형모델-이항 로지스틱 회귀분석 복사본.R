#이항 로지스틱 회귀분석/곽기영

install.packages("modeldata")
library(modeldata)
data("mlc_churn")
str(mlc_churn)
View(churn)
#전처리
churn <- mlc_churn
churn <- churn[-c(1,3)]
#결과변수, 사건 발생 0으로, 미발생 1로 간주됨
churn$churn <- factor(ifelse(churn$churn=="no",1,2),
                      levels = c(1,2),
                      labels = c("no", "yes"))
str(churn)
#데이터 분할
churn.train <- churn[1:3333,]
churn.test <- churn[3334:5000,]
#적절히 나누어졌는지
table(churn.train$churn)
prop.table(table(churn.train$churn))
table(churn.test$churn)
prop.table(table(churn.test$churn))
#결과변수가 정규분포를 따르지 않는 경우.예를 들면 지금처럼 결과변수가 범주형 변수인 경우 사용.
churn.logit <- glm(churn~.,data = churn.train,
    family = binomial(link = "logit"))
#family 인수에 결과변수의 확률분포에 해당하는 함수와 함수 내의 링크 함수를 지정.
summary(churn.logit)
#자동으로 생성된 더미 변수 명을 보면 끝에 yes가 붙여있다. /coefficients는 회귀계수를 나타낸다. 
#각 예측변수의 회귀변수는 estimate열에서 확인할 수 있다. 
#로그오즈:사건이 발생할 확률 대 사건이 발생하지 않을 확률비율의 로그값/이 회귀계수를 이용한 예측변수들의 선형결합으로 표현된다
#여기서 고객 미이탈과 고객 이탈은 각각 1과 2로 코딩되어 있기 때문에 내부적으로 고객 미이탈은 사건 미발생으로서 0으로 변환되고
#고객 이탈은 사건 발생으로서 1로 변환된다
#따라서 여기서 오즈는 고객 이탈일 확률이 미이탈일 확률의 몇 배인지를 나타낸다
#로지스틱 회귀모델에서 독립변수의 개수는 다른 독립변수의 변화가 없다는 가정하에서 해당 변수가 한단위만큼 변화할때의 
#로그오즈의 변화량을 나타낸다
#표준적인 선형회귀모델에서와 마찬가지로 +계수는 독립변수의 값이 증가할때 종속변수인 로그오즈역시 증가함을 뜻한다
#반대로 -회귀계수는 독립변수의 값이 증가할때 종속변수인 로그오즈는 감소함을 뜻한다
#하지만 로그 오즈는 그 자체로는 해석상의 어려움이 있다
#그렇기 때문에 종속변수가 로그오즈인 로지스틱 회귀 모델에 지수함수를 취해서 종속변수를 오즈로 변환합니다
#그러면 독립변수는 원래의 로지스틱 회귀 모델의 회귀계수의 지수함수를 취한 값이 된다
#이렇게 종속변수가 오즈인 회귀모델을 만들게 된다면 원래 회귀계수의 지수함수를 취해서 계산한-새롭게 계산된 회귀계수를
#이용을 해서 독립변수의 한단위 증가에 따른 오즈의 변화비율을 살펴볼 수 있다
#오즈의 변환비율=오즈비
#다른 독립변수가 동일하다는 가정하에서. 특정 독립변수의 한단위 증가에 따른 사건발생 확률 대. 미발생 확률 사이의 비율의 변화율을 의미합니다

#우리가 조금 전에 생성한 이항 로지스틱 회귀 모델을 이용해서 로즈비를 계산해보겠습니다
#coef함수를 이용해서 회귀계수를 추출한다음에 exp함수를 이용할 것임

exp(coef(churn.logit))
#범주형 변수의 경우 해석
#예를 들어. 다른 변수들이 일정하다는 가정하에서 international_planyes변수값이 1만큼 증가하면 오즈비는7.711821e+00이 됩니다
#그런데 international_planyes변수는 더미 변수로서 0이면 no를, 1이면 yes를 나타냅니다
#그래서 이 변수값이 1만큼 증가한다는 것은 international plan이 no에서 yes로 바뀐다는 것을 뜻합니다
#오즈비 7.7은 international plan이 no에서 yes로 바뀔 때 고객의 이탈 확률 대 미이탈 확률의 비가 7.7배. 약 770%커진다는 것을 의미합니다.
#voice_mail_planyes변수도 더미 변수로서 0또는 1의 값을 갖습니다. 0이면 no를, 1이면 yes를 나타냅니다. 이 변수값이 1만큼 증가하면,
#오즈비는 1.3* 10의 -1승. 즉 0.13이 됩니다.[1.319919e-01]voice_mail_planyes값이 1만큼 증가한다는 애기는 voice_mail_plan이 no에서 yes로 바뀐다는 것을 의미하고
#그렇게 voice_mail_plan이 no에서 yes로 바뀔때 오즈비 0.13은 고객의 이탈확률 대 미이탈 확률의 비가 0.13배 커진다는 것을 의미한다
#0.13배 커진다는 것은 87% 감소한다는 것을 의미합니다

#연속형 변수의 경우 해석
#예를 들어 다른 변수값들이 일정하다는 가정하에서 total_day_charge값이 1만큼 증가하면 odds비는 4.53이 됩니다 [4.539006e+00] 
#이는 고객의 이탈 확률 대 미이탈 확률의 비가 약 4.5배 커진다는 것을 의미합니다 
#만약에 total_day_charge값이 1이 아니라 2만큼 증가하면 odds 비는 4.53의 2배가 아니라 4.53의 제곱배가 됩니다[4.53^2]
#따라서 고객의 이탈 확률 대 미이탈 확률의 비가 무려 20.6배. 증가한다는 것을 알 수 있습니다
4.53^2
#이는 변수값이 커질 수록 이탈 확률 대 미이탈 확률의 비율이 지수함수적으로 증가한다는 것을 의미합니다 
#예를 들어 고객 서비스 센터에 전화하는 횟수를 나타내는 number_customer_service_calls값이 1만큼 증가하면 고객이탈 확률 대 미이탈 확률의 비는
#1.67배 증가하는데 그칩니다. 그런데 서비스 센터에 전화하는 횟수가 5만큼 증가하면 고객이탈 확률 대 미이탈 확률의 비는 13배 증가합니다
1.67^5

#로지스틱 회귀모델의 통계적 유의성을 검증해보겠습니다
#-> 로지스틱 회귀모델의 통계적 유의성은 Null deviance와 Residual deviance를 이용해서 검증할 수 있습니다
#여기서 deviance는 이탈도라고 번역합니다. 여기서 이탈도는 모델의 정확도를 나타내는 지표입니다. 엄밀히 애기하면 비적합도를 나타내는 지표 정도라 볼 수 있습니다
#왜냐하면 이탈도는 클수록 모델의 적합도가 좋지 않다는 것을 의미하기 때문입니다 
#이탈도가 작을수록 우수한 모델이라는 것을 의미합니다 
#null deviance는 null모델의 이탈도입니다. null모델은 상수항만이 포함되어 있는 예측모델을 의미합니다. 
#Residual deviance 모델은 예측변수가 모두 포함된. 현재 우리가 구축한 이항 로지스틱 회귀 모델의 이탈도 입니다
#null model과 현재 우리가 구축한 모델을 비교해보면 2758.3과 2158.7 약 600정도 더 작아진 것을 볼 수 있습니다.
#그런데 예측변수가 포함될수록 이탈도는 작아질 수 밖에 없습니다. 예측변수에 의해서 설명되는 부분이 많아지기 때문입니다.
#그런데 그 작아지는 정도가 통계적으로 의미 있는 것이 되는지가 중요합니다. 그래서 null model에 비해 개선되는 이탈도가 통계적으로 의미있는 정도의 차이인지
#검증이 필요합니다. 
#그것은 자유도의 감소폭 만큼[3332-3315]의미가 있는지 알아봐야 합니다
#null model의 자유도는 관측값의 개수에서 우리가 추정해야될 상수항의 개수를 뺀 3332입니다 참고로 관측값은 3333이었음.
#현재 우리가 구축한 예측모델은 17개의 예측변수와 더하기, 1개의 상수항. 해서 총 18개총 18개의 parameter를 추정해야 합니다
#그래서 3333관측값에서 16을 뺀 3315가 자유도가 됩니다. (33빼기 18.)
#그래서 null model과 예측모델의 자유도의 차이는 17인데. 자유도의 차이만큼 앞서 제시된 이탈도의 차이가 충분히 개선이 되어 있는지를
#통계적으로 검증을 합니다. 
#이탈도는 카이스퀘어 분포를 따릅니다. 그래서 여기 있는 이탈도를 카이스퀘어 검정 통계량으로 간주를 해서. 카이스퀘어 분포상에서 자유도의 차이만큼 충분한 크기의 이탈도 개선 효과가 통계적으로 검정을 해볼 수가 있습니다
#통계적 검정을 위해 pchisq 피카이스퀘어 함수를 이용해 검증하겠습니다.
pchisq(q=2758.3-2158.7, df=3332-3315,
       lower.tail = F)
#q인수=두개의 이탈도의 차이
#df인수=두개의 자유도의 차이
#카이 스퀘어 분포 상에서 오른쪽의 꼬리부분의 면적의 확률을 계산할 거기 때문에 lower.tail인수에 F를 지정합니다

#결과를 보면 [1.731898e-116]거의 0에 가까운. 매우 작은 유의 확률을 얻었습니다. 따라서 저러한 이탈도의 차이는 통계적으로 의미가 있는 유의한 차이다. 라고 결론을 내릴 수 있습니다. 
#그래서 회귀모델의 원소 가운데서 null.deviance에는 null모델의 이탈도가 저장이 되어 있고.deviance에는 현재 예측모델의 이탈도가 저장이 되어 있습니다. : 그래서 지정해서 구할 수 있습니다
pchisq(q=churn.logit$null.deviance - churn.logit$deviance,
       df=churn.logit$df.null - churn.logit$df.residual,
       lower.tail = F)
#동일한 결과를 얻을 수 있습니다 

churn.logit.pred <- predict(churn.logit, newdata = churn.test, type = "response")
head(churn.logit.pred)
#이제 테스트 데이터에 포함된 각 케이스의 고객 이탈 확률을 구했기 때문에 이렇게 계산된 예측 확률을 토대로 
#케이스들을 이탈 고객과 미이탈 고객으로 분류할 수 있습니다. 
#일반적으로 예측확률 50%를 판정기준으로 삼아서 예측확률이 0.5보다 큰 케이스는 이탈 고객으로 분류하고 예측확률이 0.5보다
#작거나 같은 케이스는 미이탈 고객으로 분류합니다

#사건 발생 활률로 판정된 결과를 판정결과로 변환을 해보겠습니다.
churn.logit.pred <- factor(churn.logit.pred > 0.5,
                           levels = c(F,T),
                           labels = c("no","yes"))
#참고.결과변수, 사건 발생 0으로, 미발생 1로 간주됨
head(churn.logit.pred)
#출력 결과 여섯개 모두 no인 것을 확인할 수 있습니다. 이들은 모두 고객 미이탈로 분류가 됩니다. 
#[0.07236813 0.05774332 0.22650409 0.15289153 0.07078500 0.05880824] 아까의 결과를 다시 볼때, 우린 판정 기준선 0.5가 
#너무 높게 설정되어 있음을 알 수 있습니다. 
table(churn.logit.pred)
#모든 고객 가운데 72명이 이탈 고객으로, 나머지 1595고객이 이탈 고객으로 분류됨을 알 수 있습니다. 
#실제 판정결과와 로지스틱 회귀 모델의 예측 결과를 비교해봄으로써 우리 예측모델이 얼마나 정확했는지 비교해보겠습니다. 
table(churn.test$churn, churn.logit.pred,
      dnn = c("Actual", "Predicted"))
#혼동행렬의 대각선의 값은 예측에 성공한 케이스를 나타냅니다. 실제 미이탈 고객 가운데 1414미이탈 고객으로, 실제 이탈 고객 가운데는 43이 분류가 되었습니다. 

#예측정확도를 계산해보겠습니다
mean(churn.test$churn == churn.logit.pred)
#0.8740252 87퍼센트 입니다.

#로지스틱 회귀모델 내에 유의하지 않은 변수가 포함되어 있을 수 있습니다
#현재 다루고 있는 회귀 모델을 보더라도 p value가 유의하지 않은 모델들이 다소 포함된 걸 알 수 있습니다
#예측모델을 개발하는 데 있어서 이처럼 유의하지 않은 변수를 최종모델에서 제거하는 것이 때로는 예측모델의 성능을 향상 시키는데 도움이 될 수 있습니다.
#이럴 경우 단계별 로지스틱 회귀 분석을 수행해서 유의한 변수들로만 구성된 더 작은 모델을 생성할 수 있습니다.
#step함수.


churn.logit2 <- step(churn.logit)
summary(churn.logit2)
#때로는 관심있는 특정 예측 변수가 사건 발생 확률에 미치는 영향을 살펴보는 것도 흥미롭습니다
#이를 위해서는 먼저 관심있는 예측값들이 포함된 데이터 셋을 생성을 합니다. 그 다음에 그 값들에 변화를 주어가며 어떻게 변화해가는지 추정합니다. 
#가설.
#예를 들어 고객의 서비스 센터 전화횟수가 고객 이탈 확률에 미치는 영향이 어느정도 있다고 가정해보겠습니다
#그럼 다른 예측변수들은 일정하게 고정한 채 고객의 서비스 센터 전화 횟수만을 변화시켜가면서 고객 이탈 확률이 어떻게 달라지는 지를 계산합니다.
#먼저 서비스 센터의 전화횟수의 분포를 먼저 보겠습니다.

table(churn.test$number_customer_service_calls)

#이 후 나머지 예측변수는 모두 동일한 값으로 고정을 합니다
#연속형 변수이면 평균값으로 고정하고 범주형 변수이면은 가장 낮은 범주유형으로 고정을 합니다
#이런 방식으로 먼저 테스트 데이터를 생성을 합니다.

testdata <- data.frame(number_customer_service_calls=c(0:7),
                       international_plan="no",
                       voice_mail_plan="no",
                       number_vmail_messages=mean(churn.test$number_vmail_messages),
                       total_day_charge =mean(churn.test$total_day_charge),
                       total_eve_minutes=mean(churn.test$total_eve_minutes),
                       total_night_charge=mean(churn.test$total_night_charge),
                       total_intl_calls=mean(churn.test$total_intl_calls),
                       total_intl_charge=mean(churn.test$total_intl_charge))
testdata
#다른 예측변수는 일정하게 유지한채 관심있는 예측변수만 변화하는 이 가상의 데이터 셋과
#우리가 앞서 생성한 예측 모델을 활용을 해서 사건 발생을 추정할 수가 있습니다
#추정을 위해 predict를 사용하겠습니다.
testdata$prob <- predict(churn.logit2, newdata = testdata, type = "response")
#type = "response" : type인수를 response로 지정해 활률값으로 출력되도록 하겠습니다.
#고객 이탈 확률을 서비스센터 전화횟수와 함께 출력해보겠습니다.
testdata[c("number_customer_service_calls","prob")]
#고객 이탈 확률도 커지는 것을 확인할 수가 있습니다. // 여기서 더 발전하면 어떤 양상으로 변화하는지.
#다른 예측 변수들이 일정하다는 가정하에서.[0.05881547->0.69276138]

#로지스틱 회귀분석을 수행할때는 과산포의 문제가 발생할 수 있습니다. 
#과산포의 문제는 결과변수의 실제 관측된 분산이 이항분포로부터 기대되는 분산보다 더 클때 발생합니다.
#과산포는 표준오차를 왜곡시켜 회귀 계수의 유의성 검정을 부정확하게 만들 위험이 있습니다. 
#과산포가 발생하면 glm함수를 이용하여 일반 선형모델을 생성할때 family 인수에 binomial을 지정하는 대신에 quasi"퀘이자이"바이노미얼을 지정합니다
#glm(... family = quasibinomial())

#과산포를 확인하는 방법가운데 하나는 이항분포가 적용된 로지스틱 회귀모델에서 이탈도와 자유도 간의 비율을 살펴보는 겁니다. 
#이탈도 대 자유도의 비율이 1을 크게 상회하면 과산포를 의심합니다. 
#고객 이탈 데이터에서의 과산포 비율은 이와같이 구할 수 있습니다. 

deviance(churn.logit2)/df.residual(churn.logit2)
#deviance 함수와 df.residual함수를 이용해서 이탈도와 자유도를 산출하고 그 비율을 계산합니다.
#여기서는 과산포의 비율이 작기 때문에 과산포의 위험은 없는 것으로 볼 수 있습니다. 
##0.6505038 -> 이게 적은 건지 어떻게 알지?

#과산포 여부는 또한 통계적으로 검증할 수 있습니다.
#glm함수의 family인수의 binomial을 지정한 모델과, family에 quasibinomial을 지정한. 
#두 개의 모델을 생성한 다음에 검정통계량을 계산해서 카이스퀘어 분포상에서 통계적 유의성을 검정합니다.
fit.origin <- glm(churn ~ international_plan+
                    voice_mail_plan+
                    number_vmail_messages+
                    total_day_charge+
                    total_eve_minutes+
                    total_night_charge+
                    total_intl_calls+
                    total_intl_charge+
                    number_customer_service_calls,
                  family = binomial(),
                  data = churn.train)

fit.overdis <- glm(churn ~ international_plan+
                    voice_mail_plan+
                    number_vmail_messages+
                    total_day_charge+
                    total_eve_minutes+
                    total_night_charge+
                    total_intl_calls+
                    total_intl_charge+
                    number_customer_service_calls,
                  family = quasibinomial(),
                  data = churn.train)

#이 두 모델을 바탕으로 카이스퀘어 검정 통계량을 계산하고 카이스퀘어 분포상에서 유의성을 검정합니다. 
pchisq(summary(fit.overdis)$dispersion*fit.origin$df.residual,
       fit.origin$df.residual, lower.tail = F)
# 0.08385493
#산출된 검정통계량은 과산포 비율이 1이라는 귀무가설을 검정합니다. 
#검정결과를 보면 p값은 0.084로서 유의수준 0.05에서 통계적으로 유의하지 않습니다.
#따라서 과산포의 가능성은 작다고 볼 수 있습니다.




