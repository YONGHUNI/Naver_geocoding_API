# 메인 함수
# interactive 환경(rstudio 등)에서는 function 내용 한줄한줄 실행하면 됨.
main <- function(){
    
    
    # 재현을 위한 seed
    set.seed(181196)
    
    # 사용자 정의함수가 포함된 source 코드 파일 불러오기
    if(!exists("geocoding_naver", mode="function")) source("./src/geocode.R")
    
    # 깃허브 공개 예정이라 외부에서 read
    API_credentials <- readLines("./data/secret/api_credential.txt")
    
    # 바로 문자열로 하나하나 지정해도 됨
    id <- API_credentials[1]     #혹은, id <- "yourid"
    secret <- API_credentials[2] #혹은, secret <- "yoursecret"
    
    # 샘플 데이터 생성
    # dat <- readxl::read_xlsx("./data/secret/250212.xlsx")
    # dat[sample(nrow(dat), 100), ] |> subset(select = c("법정동주소", "도로명주소", "시도", "시군구", "읍면동") ) |> 
    #      write.csv("./data/dataset.csv", fileEncoding = "cp949")
    
    # 샘플 데이터 불러오기
    dat <- read.csv("./data/dataset.csv", fileEncoding  = "cp949")
    
    # 병렬로 실행
    result_par <- geocoding_naver(df = dat, addr_colname = "법정동주소", 
                              api_key_id = id, api_key_secret = secret)
    
    
    
    # 일반 코드
    result_seq <- geocoding_naver(df = dat, addr_colname = "법정동주소", 
                    api_key_id = id, api_key_secret = secret,multiprocessing = F)
    
    
    # 병렬 코드와 결과는 같음
    identical(result,result1)
    # [1] TRUE
    
    
    # sample 저장
    write.csv(result_par,"./data/output/result_par.csv", fileEncoding = "cp949")
    write.csv(result_seq,"./data/output/result_seq.csv", fileEncoding = "cp949")

    
    # 공간자료로 변환
    library(sf)
    
    result_vect <- st_as_sf(result_par, coords = c("x", "y"), 
                            crs = 4326, agr = "constant") 
    
    # 변환 결과 도시 - tmap 사용
    tmap::tmap_mode("view")
    # 반환 결과가 정상인 행만 추출 후 도시
    tmap::qtm(result_vect[result_vect$flag=="NORMAL",])
     
    
    
    
    # 좌표계 변환
    # epsg 참고(https://sgis.kostat.go.kr/developer/html/openApi/api/dataCode/CoordCode.html) - 예시는 UTM-K
    # 정상 플래그만 변환
    result_utmk <- st_transform(result_vect[result_vect$flag=="NORMAL",],st_crs(5179))
    
    # shp로 저장
    st_write(result_utmk,"./data/output/shp/result_utmk.shp", layer_options = "ENCODING=cp949", delete_layer = TRUE)
    # st_write(result_utmk,"./data/output/shp/result_utmk.gpkg")

}





# 만약 스크립트처럼 실행한다면 <- 이걸 위한 if __name__ == '__main__'
if (!interactive()) {
    main()
}