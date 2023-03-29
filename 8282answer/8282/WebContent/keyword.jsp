<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_report.ReportDAO" %>    
<%@ page import="t_report.Report" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="bbs.BbsDAO" %>
<%@ page import="t_member.UserDAO" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>관리자_신고 관리</title>
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
        <a href="main.jsp">8282 응,답하라</a><button onclick="location='sanctions.jsp'">회원 제재 관리</button>
    </div>
    <table align="center" >
        
        
        <tbody>
            <tr><td><b>욕설</b></td>
            <%
					
				ReportDAO reportDAO = new ReportDAO();
				ArrayList<Report> list = reportDAO.getList(1);
				for(int i = 0; i<list.size(); i++) {
						
						UserDAO userDAO = new UserDAO();
						
			%>	
                <td><a href="view.jsp?num_q=<%= list.get(i).getNum_q() %>"><%= userDAO.idname(list.get(i).getId_reporter()) %><br><%= userDAO.numname(list.get(i).getNum_attacker()) %></a></td>
            <% } %>
            </tr>
            <tr><td><b>희롱</b></td>
                <%
					
				ReportDAO reportDAO2 = new ReportDAO();
				ArrayList<Report> list2 = reportDAO2.getList(2);
				for(int i = 0; i<list2.size(); i++) {
						BbsDAO bbsDAO = new BbsDAO();
						UserDAO userDAO = new UserDAO();
						
			%>	
                <td><a href="view.jsp?num_q=<%= list2.get(i).getNum_q() %>"><%= userDAO.idname(list2.get(i).getId_reporter()) %><br><%= userDAO.numname(list2.get(i).getNum_attacker()) %></a></td>
            <% } %>
            </tr>
            <tr><td><b>혐오</b></td>
                <%
					
				ReportDAO reportDAO3 = new ReportDAO();
				ArrayList<Report> list3 = reportDAO3.getList(3);
				for(int i = 0; i<list3.size(); i++) {
						BbsDAO bbsDAO = new BbsDAO();
						UserDAO userDAO = new UserDAO();
						
			%>	
                <td><a href="view.jsp?num_q=<%= list3.get(i).getNum_q() %>"><%= userDAO.idname(list3.get(i).getId_reporter()) %><br><%= userDAO.numname(list3.get(i).getNum_attacker()) %></a></td>
            <% } %>
            </tr>
            <tr><td><b>루머</b></td>
                <%
					
				ReportDAO reportDAO4 = new ReportDAO();
				ArrayList<Report> list4 = reportDAO4.getList(4);
				for(int i = 0; i<list4.size(); i++) {
						BbsDAO bbsDAO = new BbsDAO();
						UserDAO userDAO = new UserDAO();
						
			%>	
                <td><a href="view.jsp?num_q=<%= list4.get(i).getNum_q() %>"><%= userDAO.idname(list4.get(i).getId_reporter()) %><br><%= userDAO.numname(list4.get(i).getNum_attacker()) %></a></td>
            <% } %>
            </tr>
            <tr><td><b>기타</b></td>
                <%
					
				ReportDAO reportDAO5 = new ReportDAO();
				ArrayList<Report> list5 = reportDAO5.getList(5);
				for(int i = 0; i<list5.size(); i++) {
						BbsDAO bbsDAO = new BbsDAO();
						UserDAO userDAO = new UserDAO();
						
			%>	
                <td><a href="view.jsp?num_q=<%= list5.get(i).getNum_q() %>"><%= userDAO.idname(list5.get(i).getId_reporter()) %><br><%= userDAO.numname(list5.get(i).getNum_attacker()) %></a></td>
            <% } %>
            </tr>
        </tbody>
    </table>
</body>
</html>