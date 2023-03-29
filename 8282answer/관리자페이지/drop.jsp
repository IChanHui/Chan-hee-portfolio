<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_member.UserDAO" %>
<%@ page import="bbs.BbsDAO" %>
<%@ page import="t_answer.AnswerDAO" %>
<%@ page import="t_report.ReportDAO" %>
<%@ page import="java.io.PrintWriter" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Insert title here</title>
</head>
<body>
<%
	
	UserDAO userDAO = new UserDAO();
	BbsDAO bbsDAO = new BbsDAO();
	AnswerDAO answerDAO = new AnswerDAO();
	ReportDAO reportDAO = new ReportDAO();
	
	int num_m=0;
	if (request.getParameter("num_m") != null) {
		num_m = Integer.parseInt(request.getParameter("num_m"));
	}
	userDAO.drop(num_m);
	bbsDAO.drop(num_m);
	answerDAO.drop(num_m);
	reportDAO.drop1(userDAO.numid(num_m));
	reportDAO.drop(num_m);
	
	PrintWriter script=response.getWriter();
	script.println("<script>");
	script.println("alert('해당 계정이 삭제되었습니다.')");
	script.println("history.back()");
	script.println("</script>");
	
	
%>

</body>
</html>