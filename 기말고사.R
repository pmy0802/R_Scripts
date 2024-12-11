# 가위바위보 실험(시뮬레이션)

# 변수초기화
iteration <- 1000  # 반복횟수
prob <- NULL       # 누적비율 변수
tie_count <- 0     # 비긴 횟수 저장 변수

# 가위바위보 던지기 반복문
for (x in 1:iteration) {
  # 가위바위보 선택 (사용자와 컴퓨터가 각각 1회 선택)
  user <- sample(c("가위", "바위", "보"), 1, replace = TRUE)
  computer <- sample(c("가위", "바위", "보"), 1, replace = TRUE)
  
  # 비기는 경우 체크
  if (user == computer) {
    tie_count <- tie_count + 1
  }
  
  # 누적비율 추가
  prob <- c(prob, round(tie_count / x, 2))
}

# 누적비율 값을 사용하여 데이터프레임으로 변환
df.rps <- data.frame("반복수" = 1:iteration, "누적비율" = prob)

# 실험결과 그래프
library(ggplot2)


ggplot(data = df.rps, aes(x = 반복수, y = 누적비율, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point() +
  geom_hline(yintercept = 1/3, color = "red") +  # 비길 확률 이론값 = 1/3
  labs(
    title = "가위바위보 비길 확률의 누적 비율 변화",
    x = "반복수",
    y = "비길 확률 누적비율"
  )
