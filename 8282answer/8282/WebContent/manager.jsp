<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>관리자 페이지</title>
  <style>
        body{ 
            text-align: center;
            margin-top: 15%;
         }

        a{ font-size: 50px;  }
        a:link{ color: black; text-decoration: none; }
        a:visited{ color: black; }
        a:hover{ color: blue;}

        button{
            margin-top: 50px;
            font-size: 30px;
        }
    </style>
</head>
<body>
  <b><a href="main.jsp">8282 응,답하라</a></b><br><br>
  <button onclick="location='keyword.jsp'">신고 키워드</button>&emsp;<button onclick="location='sanctions.jsp'">회원 제재 관리</button>
</body>
</html>