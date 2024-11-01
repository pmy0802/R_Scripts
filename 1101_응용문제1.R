# 미세먼지 또는 초미세먼지 농도 웹스크래핑 추출
url = "https://www.airkorea.or.kr/web/sidoQualityCompare?itemCode=10008&pMENU_NO=102"
html = read_html(url, encoding = "utf-8")

dust = html_nodes(html, "#sidoTable_thead")%>%
  html_text()

dust

# 특수 문자열들을 ""(empty string)대체
titles = gsub("\r|\n|\t", "", titles)

titles