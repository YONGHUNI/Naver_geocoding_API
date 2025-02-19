
# 1. 병렬 처리를 위한 함수
mp_init <- function(n_processes = parallel::detectCores(logical = F)  ){
    # 필요한 패키지 확인 및 로드
    require(doParallel)  
    
    
    # 논리적인 것을 제외한 코어 수 - 1개만큼 프로세스 생성
    no_cores <- n_processes  
    cl <- makeCluster(no_cores, type="PSOCK")  
    
    # 설정 리턴        
    return(cl)
    
}



# 2. get 메서드를 사용한 지오코딩 API
# 3개 인자: 주소(문자열), API ID(문자열), API secret(문자열)
# 반환: 지오코드 결과 R list(다른 언어의 object라고 봐도 무방)
API_get_geocode <- function(addr,clientId,clientSecret){
    
    # 필요한 패키지 확인 및 로드
    require(httr)
    
    # !!!메뉴얼 참고!!!
    # https://api.ncloud-docs.com/docs/ainaverapi-maps-overview#%EC%9A%94%EC%B2%AD%ED%97%A4%EB%8D%94
    
    # API를 요청하기 위한 뼈대 주소
    API_URL <- "https://naveropenapi.apigw.ntruss.com/map-geocode/v2/geocode" 
    
    # get 메서드로 요청
    result <- GET(
                  # API URL
                  url = API_URL,
                    
                  # 헤더에 인증 정보 적재 - 네이버 지오코딩 메뉴얼 참고
                  add_headers(
                      `X-NCP-APIGW-API-KEY-ID` = clientId, 
                      `X-NCP-APIGW-API-KEY` = clientSecret
                  ),
                  
                  # https://api.ncloud-docs.com/docs/ai-naver-mapsgeocoding-geocode
                  # query할 내용을 R 자료구조 list에 적재
                  # 아래와 같이 list에 추가 가능.
                  # 추가 가능한 query는 위 메뉴얼 참고
                  query = list(
                      `query` = addr
                      #`coordinate` = coord,
                      # ...
                              )
                )
    
    # get 메서드로 요청한 결괏값 반환
    return(result)
    
    
}


# 3. API 반환 결과를 정제하는 함수
# 2개 함수 인자: API 응답(R list), API 호출에 사용한 주소가 가진 속성 정보 i.e., 데이터 프레임에서 호출에 사용한 주소가 위치한 row의 모든 정보
# 반환: 1개 row만을 지니는 데이터 프래임, 사용한 주소가 위치한 row의 모든 정보와 API 응답으로부터 추출한 X,Y 좌푯값, 그리고 API 응답 정상/비정상인지 판별 flag 포함

API_data_cleaner <- function(API_RESULT,ith_df){
    
    require(XML)
    
    # API 반환 자료에서 내용만 추출 후 raw byte를 문자열로 변환
    result <- base::rawToChar(API_RESULT$content)  
    
    # encoding을 UTF-8로
    base::Encoding(result) <- "UTF-8"
    
    # 반환 형식이 XML이므로 XML을 R 자료형으로 변환
    list <- XML::xmlToList(result)
    
    # 만약 반환된 자료에 좌푯값이 없으면
    if (base::is.null(x = list$addresses$x)){
        
        
        # X, Y 좌표에 -9999999 할당, flag에 메시지 적재
        x <- -1e7+1
        y <- -1e7+1
        flag <- "http get request sucessful, address not returned"
    
    # 아니면 좌푯값이 잘 있다면
    } else {
        
        # 숫자형으로 변환 후 x, y 좌표 추출, flag에 성공 메시지 적재
        x <- base::as.numeric(list$addresses$x)
        y <- base::as.numeric(list$addresses$y)
        flag <- "NORMAL"
        
    }
    
    
    # 여러 값(column)들을 하나로 묶어준 후 데이터프래임 자료형으로 변환
    ret <- as.data.frame(cbind(ith_df,x,y,flag))
    
    # 변환한 데이터프래임을 반환, 1개 row를 가짐
    return(ret)
}


# 4. API 반환 상태 정보에 따른 health check 함수
# 인자: API 반환 값(R list), 데이터 프레임에서 호출에 사용한 주소가 위치한 row의 모든 정보(dataframe)
# 반환: API 반환 상태 정보를 포함한 API 결과 dataframe(1개 관측치-row)
API_health_chk <- function(API_RESULT, ith_df){
    
    
    # 만약 API 상태 코드가 200(정상임)
    if (API_RESULT$status_code == 200L){
        
        # 3. 에서 정의한 함수 호출
        ret <- API_data_cleaner(API_RESULT, ith_df)
        
    # 아니라면, X, Y 좌표에 -9999999 할당, flag에 메시지 적재
    }else if (API_RESULT$status_code == 400L){
        
        x <- -1e7+1
        y <- -1e7+1
        flag <- "http-get: Bad Request Exception, 400"
        ret <- as.data.frame(cbind(ith_df,x,y,flag))
        
    # 아니라면, X, Y 좌표에 -9999999 할당, flag에 메시지 적재        
    }else if (API_RESULT$status_code == 500L){
        
        x <- -1e7+1
        y <- -1e7+1
        flag <- "http-get: Unexpected Error, 500"
        ret <- as.data.frame(cbind(ith_df,x,y,flag))
        
     # 아니라면, X, Y 좌표에 -9999999 할당, flag에 메시지 적재   
    }else{
        
        x <- -1e7+1
        y <- -1e7+1
        flag <- "http-get: Unknown Error"
        ret <- as.data.frame(cbind(ith_df,x,y,flag))
    }
    
    
    # 결괏값 리턴
    return(ret)
    
}



# 5. 네이버 지오코딩 함수
# 인자: 문자열로 주소가 포함된 df(dataframe), 주소가 위치한 열의 이름(문자열), API id(문자열), API secret(문자열), 병렬처리 여부(bool, 기본값은 TRUE)
# 반환: 인자로 투입한 df에 지오코딩 결과 x, y 좌표, flag 포함한 데이터프레임
# 위에서 정의한 1.2.3.4. 함수 모두를 사용
geocoding_naver <- function(df, addr_colname, api_key_id, api_key_secret, multiprocessing = T ){
    
    # 필요한 패키지 확인 및 로드
    require(doParallel)
    require(httr)
    
    
    
    # 투입한 열의 이름이 df의 모든 열이름 중 하나라도 일치하지 않거나 동일한 열 이름이 있으면 에러 메시지
    if ( sum( names(df) == addr_colname) != 1 ) stop("name of the address column should be checked")
    
    # 문자열 배열로 df로부터 주소 열을 추출
    addr_arr <- subset(df,select = addr_colname)[[1]]
    
    
    # 만약 병렬처리 여부가 참일 경우(기본 설정),
    if (multiprocessing) {
        
        # 만약 주소의 수가 50개보다 적을 경우 다음의 메시지 표출
        # 주소의 수가 적을 경우 병렬 처리의 의미가 떨어질 수 있음을 경고
        if (length(addr_arr) < 50) cat("Warning: Multi-Processing can be more expensive... Consider not using it for small datasets.\n")
        
        # 1.에서 정의한 함수를 스케줄러에 등록함
        cluster <- mp_init()
        registerDoParallel(cluster)  

        # 병렬 처리를 위해 여러 child R session을 만드는데, 거기서 사용해야할 패키지 및 사용자 정의함수 명시
        pass_in_pkg <- c("httr","XML")
        pass_in_fn <- c("API_data_cleaner","API_get_geocode","API_health_chk")
        
        
        
        # 병렬처리 시작 for loop와 유사
            # 인자 설명
            # i: for loop의 그것과 동일, 
            # .combine: 병렬 처리 후 데이터를 합칠 함수 정의. df의 행 단위로 연산이 이루어지기 때문에 결국 row bind를 수행하면 됨.
            # .packages: child 세션에서 사용할 패키지
            # .export: 본 세션에서 정의한 함수들을 적재
        result <- foreach(i=1:length(addr_arr), .combine = rbind, .packages = pass_in_pkg, .export = pass_in_fn) %dopar% {
            
            # 2.에서 정의한 함수 호출
            API_get_result <- API_get_geocode(addr_arr[i], api_key_id, api_key_secret)
            
            # 4.에서 정의한 함수 호출
            API_cleaned <- API_health_chk(API_get_result, df[i,])
            
            
            # 결괏값 반환
            return(API_cleaned)
            
        } # end dopar - parallel code
        
        
        
        # 병렬처리 clean up - 사용한 child R 세션 정리
        stopCluster(cluster)
        
        
    # 만약 병렬처리를 하지 않는다면,
    } else {
        
        
        # for loop 사용하는 것 외에는 위와 동일
        for (i in 1:length(addr_arr)) {
            
            API_get_result <- API_get_geocode(addr_arr[i], api_key_id, api_key_secret)
            
            if (i==1) {
                
                result <- API_health_chk(API_get_result, df[i,])
                
            } else {
                
                result <- rbind(result, API_health_chk(API_get_result, df[i,]))
                
            } #end inner if
            
        }  #end for - sequential code
        
    } # end if
    

    
    # 지오코딩 결괏값 반환
    return(result)
    
    
}


