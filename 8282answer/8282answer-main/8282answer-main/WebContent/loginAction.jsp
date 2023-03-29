<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_member.UserDAO" %>
<%@ page import="java.io.PrintWriter" %>
<% request.setCharacterEncoding("UTF-8"); %>
<jsp:useBean id="user" class="t_member.User" scope="page"/>
<jsp:setProperty name="user" property="id" />
<jsp:setProperty name="user" property="password" />
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
		if (id != null) {
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('이미 로그인이 되어있습니다.')");
			script.println("location.href='main.jsp'");
			script.println("</script>");
		}
	
		UserDAO userDAO = new UserDAO();
		int result = userDAO.login(user.getId(), user.getPassword());
		if (result == 1 ){
			session.setAttribute("id",user.getId());
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("location.href='main.jsp'");
			script.println("</script>");
		}
		else if (result == 0 ){
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('비밀번호를 확인해 주세요.')");
			script.println("history.back()");
			script.println("</script>");
		}
		else if (result == -1 ){
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('아이디를 확인해 주세요.')");
			script.println("history.back()");
			script.println("</script>");
		}
		else if (result == -2 ){
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('데이터베이스 오류가 발생했습니다.')");
			script.println("history.back()");
			script.println("</script>");
		}
	%>
</body>
</html>