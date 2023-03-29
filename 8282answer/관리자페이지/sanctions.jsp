<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_member.UserDAO" %>
<%@ page import="t_member.User" %>
<%@ page import="java.util.ArrayList" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>회원 관리 제재</title>
<style>
    a{ font-size: 30px; }
    a:link{ color: black; text-decoration: none; }
    a:visited{ color: black; }
    a:hover{ color: blue;}

    button{ font-size: 20px; float: right;}

    table{ 
        margin-top: 60px; 
        border-spacing: 30px;  
    }
    thead{ 
        font-size: 30px;
        border: 2px solid black; 
    } 
    tbody{ text-align: center;}

</style>
</head>
<body>
<div>
        <a href="main.jsp">8282 응,답하라</a><button onclick="location='manager.jsp'">뒤로 가기</button>
    </div>
    <table align="center" >
	<thead><td><b>닉네임</b></td><td><b>신고 수</b></td><td></td></thead>
	<tbody>
		<%
			UserDAO userDAO = new UserDAO();
			ArrayList<User> list = userDAO.reportList();
			for(int i=0; i<list.size(); i++){
				
			int reporttotal = list.get(i).getReport_a() + list.get(i).getReport_q();
		%>
			<tr>
				<td><%= list.get(i).getName() %></td>
				<td><%= reporttotal %></td>
				<td><button onclick="location='drop.jsp?num_m=<%= list.get(i).getNum_m()%>'">계정 탈퇴</button></td>
			</tr>
		<%
			}
		%>
	</tbody>
	</table>
</body>
</html>