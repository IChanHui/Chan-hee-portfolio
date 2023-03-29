<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_member.UserDAO" %>
<%@ page import="java.io.PrintWriter" %>
<% request.setCharacterEncoding("UTF-8"); %>
<jsp:useBean id="user" class="t_member.User" scope="page"/>
<jsp:setProperty name="user" property="id" />
<jsp:setProperty name="user" property="password" />
<%@ page import="t_answer.Answer" %>
<%@ page import="t_answer.AnswerDAO" %>
<%@ page import="recommend.RecommendDAO" %>

<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>8282응답하라</title>
</head>
<body>
	<%
	String id=null;
	if (session.getAttribute("id") != null ){
		id = (String) session.getAttribute("id");
	}
	if (id == null) {
		PrintWriter script=response.getWriter();
		script.println("<script>");
		script.println("alert('로그인을 하세요.')");
		script.println("location.href='login.jsp'");
		script.println("</script>");
	} else {
		AnswerDAO answerDAO = new AnswerDAO();
		String i = request.getParameter("num_a");
		int num_a = Integer.parseInt(i);
		
		RecommendDAO recommendDAO = new RecommendDAO();
		int value= recommendDAO.getvalue(num_a,id);
		if (value == 1){
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('이미 추천이 되었습니다.')");
			script.println("history.back()");
			script.println("</script>");
		}else {
			answerDAO.recommend(num_a);
			//recommendDAO.valueup(num_a,id);
			recommendDAO.input(num_a,id);
			UserDAO userDAO = new UserDAO();
			userDAO.inputpoint(userDAO.numatom(num_a));
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('추천이 되었습니다.')");
			script.println("location.href = 'main.jsp'");
			script.println("</script>");
		}
		
		
		
		
		
		
	}
	%>
</body>
</html>