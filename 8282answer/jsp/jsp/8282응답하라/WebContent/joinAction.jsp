<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_member.UserDAO" %>
<%@ page import="java.io.PrintWriter" %>
<% request.setCharacterEncoding("UTF-8"); %>
<jsp:useBean id="user" class="t_member.User" scope="page"/>
<jsp:setProperty name="user" property="id" />
<jsp:setProperty name="user" property="password" />
<jsp:setProperty name="user" property="name" />
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
	
		if (user.getId() == null || user.getPassword() == null || user.getName() == null) {
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('입력이 안 된 사항이 있습니다.')");
			script.println("history.back()");
			script.println("</script>");
		} else {
			UserDAO userDAO = new UserDAO();
			int result = userDAO.join(user);
			if (result == -1 ){
				PrintWriter script=response.getWriter();
				script.println("<script>");
				script.println("alert('.')");
				script.println("history.back()");
				script.println("</script>");
			}
			else {
				session.setAttribute("id",user.getId());
				PrintWriter script=response.getWriter();
				script.println("<script>");
				script.println("location.href = 'main.jsp'");
				script.println("</script>");
			}
		}
		
		
	%>
</body>
</html>