install.packages("dplyr")
install.packages("gapminder")

library(dplyr)
library(gapminder)

# 시계열데이터 그래프 애니메이션
# 전세계 국가(핀란드, 대한민국, 베트남)에 대한 연도별 기대수명과 국내총생산(GDP)와의 관계

gapminder

# 전세계 국가 중에서 3개국(핀란드, 대한민구, 베트남) 데이터만 필터링해서 추출
df = gapminder %>% filter(country=="Finland" | country=="Korea, Rep." | country=="Vietnam")

df

# 시계열데이터 그래프
# x축: 1인당 총생산, y축: 기대수명
# 애니메이션 설정 추가
anim=ggplot(df, aes(x=gdpPercap, y=lifeExp, size=pop, colour=country))+
  geom_point(alpha=0.5)+
  scale_size(range = c(5, 15))+
  labs(title = "연도: {frame_time}", x="1인당 GDP", y="기대수명")+
  transition_time(year)+
  shadow_wake(0.5)

animate(anim, width=500, height=400, duration = 10, renderer = gifski_renderer(loop=FALSE) )

# 그림애니메이션: 양궁
install.packages("magick")
library(magick)

# 스크립트와 동일 폴더에 있는 이미지를 읽어오기
bg = image_read("D:/박명연/RProject/background.png") #배경이미지
target = image_read("D:/박명연/RProject/target.png") #과녘판이미지
arrow = image_read("D:/박명연/RProject/arrow.png") #화살이미지

print(bg)
print(target)
print(arrow)

# 이미지 크기 조정
bg = image_scale(bg, "600x300!")
target = image_scale(target, "80x170!")
arrow = image_scale(arrow, "100x25!")

print(bg)
print(target)
print(arrow)

# 이미지 회전
arrow = image_rotate(image_background(arrow, "none"), -11)
print(arrow)

# 이미지 합성
bg2 = image_composite(bg, target, offset = geometry_point(450,80))
print(bg2)

# 화살 위치 초기화
x = 0
y = 220

# 반복문을 사용하여 화살이 움직이는 애니메이션 설정
# 반복문이 수행될 때마다 x축의 값은 20 증가시키고 y축의 값은 -4
while(TRUE){
  
  # 화살이미지 위치(x, y)
  position = geometry_point(x, y)
  
  # 이미지 합성: bg2(배경+과녘판) + arrow(화살)
  img = image_composite(bg2, arrow, offset=position)
  
  print(img)
  
  Sys.sleep(0.1)
  
  # x축의 값이 400이 되면 반복문 빠져나간다.
  if (x == 400)
    break
  
  x = x + 20
  y = y - 4
  
}

# 그림애니메이션을 이용하여 직선이 아닌 포물선으로 날아가도록 수정해보세요.

# 화살 위치 초기화
x_start = 0
y_start = 220
angle_start = -45  # 시작 각도 (도)

# 포물선 매개변수 설정
a = 0.005  # 포물선의 곡률
x_target = 400  # 목표 x 좌표

# 화살의 위치와 각도를 계산하는 함수
calculate_position <- function(t) {
  x = x_start + t
  y = y_start - (a * t^2)
  angle = atan2(-2 * a * t, 1) * 180 / pi  # 각도를 도(degree)로 변환
  return(list(x = x, y = y, angle = angle))
}

# 배경 이미지와 화살 이미지 로드 (이미지 파일 경로를 적절히 수정해야 합니다)
bg2 = image_read("background.png")
arrow = image_read("arrow.png")

# 애니메이션 프레임 생성
for (t in seq(0, x_target, by = 10)) {
  pos = calculate_position(t)
  
  # 화살 이미지 회전
  rotated_arrow = image_rotate(arrow, pos$angle)
  
  # 화살 이미지 위치 설정
  position = geometry_point(pos$x, pos$y)
  
  # 이미지 합성: bg2(배경+과녁판) + rotated_arrow(회전된 화살)
  img = image_composite(bg2, rotated_arrow, offset = position)
  
  # 이미지 출력
  print(img)
  
  # 애니메이션 속도 조절
  Sys.sleep(0.05)
  
  # 목표 지점에 도달하면 반복 종료
  if (pos$x >= x_target) break
}

# 마지막 프레임 표시 (화살이 과녁에 꽂힌 상태)
Sys.sleep(1)





