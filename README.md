# house-prices

## R 코드 파일

**scripts/01_preprocessing.R**: 데이터 전처리

**scripts/02_analysis.R**: 여러 모델을 검증 데이터에서 비교해서 예측 모델 결정

**scripts/03_final-prediction.R**: 랜덤포레스트로 테스트 데이터 예측

**src/functions.R**: `02_analysis.R`과 `03_final-prediction.R`에서 공통으로 사용되는 함수


## 예측 성과(RMSE of `log(SalePrice)`)

* 검증 데이터
    1. LASSO: 0.184
    2. Random Forest: 0.153
* 테스트 데이터
    1. Random Forest: 0.151
