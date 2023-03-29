<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_report.ReportDAO" %>
<%@ page import="t_member.UserDAO" %>
<%@ page import="java.io.PrintWriter" %>
<jsp:useBean id="t_report" class="t_report.Report" scope="page"/>
<jsp:setProperty name="t_report" property="keyword" />
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>신고하기</title>
</head>
<body>
<%
		String id = null;
		if (session.getAttribute("id") != null){
			id = (String) session.getAttribute("id");
		}
		if (id == null) {
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('로그인을 하세요.')");
			script.println("location.href='login.jsp'");
			script.println("</script>");
		} else {
			if(t_report.getKeyword() != 0){
				ReportDAO reportDAO = new ReportDAO();
				int qqq = (Integer)session.getAttribute("num_q");
				int ppp = (Integer)session.getAttribute("num_attacker");
				
			
				reportDAO.report(id,qqq,0,ppp,t_report.getKeyword());
				
				UserDAO userDAO = new UserDAO();
				userDAO.updateReport_a(ppp);
				
				
				PrintWriter script=response.getWriter();
				script.println("<script>");
				script.println("alert('신고 되었습니다.')");
				script.println("location.href='main.jsp'");
				script.println("</script>");
			} else {
				PrintWriter script=response.getWriter();
				script.println("<script>");
				script.println("alert('신고 키워드를 선택하세요.')");
				script.println("history.back()");
				script.println("</script>");
			}
		}
		
		
		
%>
</body>
</html>