<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="java.io.PrintWriter" %>
<%@ page import="t_report.ReportDAO" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
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
			script.println("alert('로그인 후 이용해 주세요')");
			script.println("location.href='login.jsp'");
			script.println("</script>");
		} else {
			String i = request.getParameter("num_attacker");
			int num_attacker = Integer.parseInt(i);
			session.setAttribute("num_attacker", num_attacker);
			
		}
%>
		신고 하시는 이유를 선택 해 주세요.
		<form method="post" action="reportAction_q.jsp">
		<label><input type="radio" name="keyword" value=1>#욕설</label>
		<label><input type="radio" name="keyword" value=2>#희롱</label>
		<label><input type="radio" name="keyword" value=3>#혐오</label>
		<label><input type="radio" name="keyword" value=4>#루머</label>
		<label><input type="radio" name="keyword" value=5>#기타</label>
				
		<input type="submit" class="btn btn-primary pull-right" value="신고하기">
		</form>
</body>
</html>