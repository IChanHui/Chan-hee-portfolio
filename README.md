## Chanhee-Lee Portfolio Google Slides     
URL : <https://docs.google.com/presentation/d/15TdmoTVXCpN_JbZ4lvZ4Ide0qCHyDeONcq8cqH_6qWA/edit?usp=sharing>       


## bunnyburrow22st : 마음케어 서비스를 운영하는 커뮤니티 & NFTs      
>URL back :<https://github.com/IChanHui/Chan-hee-portfolio/tree/main/Guiding-Cody-Bot/haskell-lambda/test>      

**haskell backend 작업**      
커뮤니티에 필요한 디스코드 봇의 기능을 만드는 작업을 하였으며 Haskell로 Backend 작업을 진행하였습니다.      
테스트 전용 AWS Lambda (with API Gateway)를 따로 만들어 haskell .zip을 업로드해서         
API recose 또는 client(디스코드)를 이용하여 테스트 후        
release 전용 Lambda에 테스트가 완료된 .zip을 업로드합니다.      

release후에도 디스코드 서버 내에 한번 더 테스트를 할 수 있기에 2번째 테스트를 거친 후      
기능에 아무 문제가 없다고 판단이 되면 기능을 커뮤니티에 release 합니다.      

client(디스코드)와 Lambda는 Restfull API로 연결했으며      
AWS 내에 Lambda와 Event Bridge는 HTTP로 연결했습니다.     
  
DynamDB는 Haskell 내에 라이브러리로 연결하여 사용했으며       
S3는 서비스 내에서 생성해주는 URL을 Haskell 내에 하드코딩하여 사용했습니다.     
DynamoDB에 S3 URL을 저장하여 서비스에 활용하기도 했습니다.     
      

----





## ADIO : 메타버스에 블록체인을 활용한 광고(AD)를 달아주고 유저가 광고를 시청하면 토큰을 얻을 수 있는 서비스       
>URL front : <https://github.com/IChanHui/Chan-hee-portfolio/tree/main/ADIO/front-end/images>      
>URL back : <https://github.com/IChanHui/Chan-hee-portfolio/tree/main/ADIO/back-end/haskell-lambda/test>

**frontend 및 backend 작업**
서비스를 보여주기 위한 시뮬레이션 데모을 웹으로 제작하였으며      
BNB chain을 적용하여 Javascript, HTML, CSS를 사용해 페이지에  제작하고 vercel에 gitHub를 연결하여 웹 배포를 진행했습니다.      
API는 AWS Lambda (with API Gateway)를 Restful로 연결했으며  Haskell .zip을 업로드하여 사용했습니다.      
front에 사용될 리소스를 local로 저장하는 것 대신 S3(URL)을 이용하여 front 작업을 했으며       
서비스의 Demo 테스트를 위해 DynamoDB를 연결하여 사용했습니다.      


----
     







