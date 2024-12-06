# 피마 인디언 당뇨병 예측

<br>

## 진행기간
> **2022/09~12**
<br>


## 사용 데이터
> **Kaggle에서  제공하는 피마인디언 당뇨 데이터**

> https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database?resource=download
<br>

## 개발 환경
> **R**
<br>

### 역할
> **EDA, Modeling, ppt 작성**
<br>

## 분석 주제
> **피마 인디언 당뇨병 예측**
<br>


## 분석 요약

1. EDA
    1. 변수 선택(Levene’s test, Shapiro Wilks test, Student t-test, Wilcoxon rank sum test)
    2. 결측치 처리(Mice)
2. Modeling
    1. 예측 모델(NB, LR, DT, KNN, SVM, RF, XGB, Ensemble)
    2. Cutoff 설정
3. Model Selection(ROC plot, AUC)
<br>

## 분석 과정

### *EDA*
<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/3d1afacb-f81a-4c61-9e04-ed66f2c69d43" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/91384fd8-89a5-4e1c-acf9-b7c0b754c299" width="95%">
    </td>
  </tr>
</table>
<br>

- `평균 비교 검정`을 진행하여 유의한 변수를 선택(target은 당뇨병 여부)
    - 검정을 진행하기 전 `등분산성`을 확인

<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/6e9fe38c-1239-4fb1-95cb-996d6f81b661" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/43091a02-5f4c-4587-9822-89a45464bbd9" width="95%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="center">
      <img src="https://github.com/user-attachments/assets/e33ce433-4898-417e-bedd-d775a2ceb52b" width="60%">
    </td>
  </tr>
</table>
<br>

- 모든 변수를 위와 같은 절차와 시각화를 통해 `Feature selection` 진행
    - 결과적으로 모든 변수에서 집단 간 차이가 유의했음
- 상관관계를 살펴보았을 때, 상당히 높게 나오는 변수가 있어 선형 모델 사용 시 유의

<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/015ab6f9-dd8e-45e1-babf-df603fc2d973" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/e38abff5-e484-4721-86dd-fdf8453aad74" width="95%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="center">
      <img src="https://github.com/user-attachments/assets/fb1cb0c0-d04c-42dd-b815-960cd49810ef" width="60%">
    </td>
  </tr>
</table>
<br>

- 결측 비율이 20% 이상인 경우가 있어 `회귀 기반 결측치 대체` 방법 사용
    - 평균값 대체와 트리 기반 대체 방법도 사용해보았으나 가장 성능이 좋았던 `Mice 기법` 선택
- Mice는 변수들이 정규분포를 따르지 않아도 되며 앞선 검정에서 정규분포를 따르지 않은 변수가 많아 적합한 대체 방법이라고 판단
    - Fill-in 과 Imputation의 순서로 결측치 대체가 이루어지며 설정한 개수만큼 데이터 셋이 만들어집니다. 이후 각 데이터셋에 모델링을 진행하고 Rubin’s Rule을 이용하여 각 모수 추정을 이루는게 일반적
    - 이 방법에 제한이 있어 각 데이터셋에서 대체된 결측들을 평균을 내어 결측치가 대체된 한 개의 데이터셋을 생성하여 진행

<br>

### *Modeling*
<br>

- 다양한 지표를 비교할 수 있으나 여기서는 Cutoff에 따른 recall, precision, Accuracy를 중심으로 살펴봄
- 아래에서 언급하지 않은 모델도 같은 절차를 따라 분석함

<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/76ab31fd-d226-4619-ba31-8f82a46576d4" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/8a5824ab-1d4d-43aa-b65c-6dca87bffd41" width="95%">
    </td>
  </tr>
</table>
<br>

- 다중공선성을 확인하기 위해 VIF를 살펴본 결과 크게 문제되는 변수는 없었음
- 오즈비를 나타낸 그래프를 확인해보면 혈장 포도당 농도, BMI, 임신횟수가 가장 유의했으며 나머지 변수는 신뢰구간에 1이 포함되어 유의하지 않은 것으로 판단
    - 앞선 검정에서 모든 변수가 두 집단간 차이가 유의함으로 나타났지만 이러한 결과가 나온 이유는 정규성을 가정해야하는 모델이며 교호작용항을 고려하지 않아서 도출된 결과로 보임

<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/f8930c2b-b831-4a48-a85e-1038f8db3683" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/eecfb1de-51bc-487b-864a-39fd61b79fc5" width="95%">
    </td>
  </tr>
</table>
<br>

- 왼쪽 그래프는 k의 개수에 따른 오분류을 나타낸 그래프. 이에 k는 15일 때 최적
- 하지만 cutoff에 따른 지표를 살펴보았을 때, knn 모형은 적합하지 않은 것으로 확인

<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/a556796c-12b7-49f2-9ad9-5d7d869b1293" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/1c8cd8ba-d8f0-49fe-ac83-e7ecdbd15b81" width="95%">
    </td>
  </tr>
</table>
<br>

- 변수 중요도를 보면 인슐린, 혈장 포도당 농도, BMI 등의 순서로 나타나는 것을 확인

<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/4b3c5ef5-8e53-41e1-8383-5c9c3bdab289" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/2f4ebee2-c513-49a8-8d74-fa4f7b791757" width="95%">
    </td>
  </tr>
</table>
<br>

- Soft Voting 방법으로 NB, LR, RF, XGB를 활용한 결과 가장 높은 성능
    - Decision Tree의 경우 랜덤 포레스트와 비슷한 결이므로 제외했고, KNN은 적합하지 않은 모형이었으며 svm은 확률이 도출되지 않기 때문에 제외
<br>

<table width="100%">
  <tr>
    <td align="center">
      <img src="https://github.com/user-attachments/assets/5d6890f1-9d47-41d9-8935-2db48b79c83f" width="70%">
    </td>
  </tr>
</table>
<br>

- svm을 제외하고서 모든 모형의 ROC curve
- Accuracy 하나의 값을 보고 모형의 성능을 평가할 수 도 있으나 Cutoff에 따라 서로 다른 예측확률을 가지기에 모델 성능 비교에 어려움을 겪을 수 있음
    - `ROC curve` 와 `AUC`를 확인하여 이를 해결
- ROC plot을 확인했을 때 KNN과 결정 트리를 제외하고서 전체적으로 좋은 모형으로 해석됨
- AUC 값으로는 로지스틱과 앙상블이 높은 값을 보이는데 해석의 용이성을 우선이라면 LR을, 성능이 우선이라면 Ensemble을 권장
    - 로지스틱의 경우 정규성 가정, 다중공선성 등 고려할 것이 많으며 교호작용항을 추가할시 해석에 복잡성이 증가할 수 있음

<br>

## 시사점 및 보완할 점

- `t-test`, `Wilcoxon rank sum test` 등과 같은 `평균 비교 검정`으로 변수 선택
- 결측치 대체 방법 중 `Mice`를 사용
    - 회귀 기반으로 좀 더 세밀하게 결측을 대체하여 다른 방법보다 더 좋은 성능 도출
    - Rubin’s Rule을 이용하여 모수 추정하는 과정이 제대로 이루어지지 않아, 평균을 내는 방법을 사용
- `Cutoff`에 따라 지표 성능 확인
    - 이 과정에서 데이터에 적합하지 않은 모형 탐색 및 최적의 Cutoff 도출
- 최종적으로 `Soft Ensemble`을 활용하여 최적의 모델을 도출




