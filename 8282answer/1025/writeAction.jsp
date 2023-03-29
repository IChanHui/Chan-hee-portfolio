<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="bbs.BbsDAO" %>
<%@ page import="t_member.UserDAO" %>
<%@ page import="java.io.PrintWriter" %>
<%@ page import="t_stop.StopDAO" %>
<% request.setCharacterEncoding("UTF-8"); %>
<jsp:useBean id="bbs" class="bbs.Bbs" scope="page"/>
<jsp:setProperty name="bbs" property="title" />
<jsp:setProperty name="bbs" property="content_q" />
<jsp:setProperty name="bbs" property="category" />
<jsp:useBean id="user" class="t_member.User" scope="page"/>
<jsp:setProperty name="user" property="num_m" />


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
		//수정
		
		
		if (id == null){
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('로그인을 하세요.')");
			script.println("location.href='login.jsp'");
			script.println("</script>");
		} else {
		
			StopDAO stopDAO = new StopDAO();
			BbsDAO bbsDAO = new BbsDAO();
			int num_m= bbsDAO.idnum(id);
			
			int a = stopDAO.confirm(num_m,0);
		if (a != -1) {
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('질문작성이 정지된 계정입니다.')");
			script.println("location.href='main.jsp'");
			script.println("</script>");
			
			
		} else {
			if (bbs.getTitle() == null || bbs.getContent_q() == null) {
				PrintWriter script=response.getWriter();
				script.println("<script>");
				script.println("alert('입력이 안 된 사항이 있습니다.')");
				script.println("history.back()");
				script.println("</script>");
			} else {
			//bbsdao int num 삭제
				int result = bbsDAO.write(bbs.getTitle(), num_m, bbs.getContent_q(), bbs.getCategory());
				if (result == -1 ){
					PrintWriter script=response.getWriter();
					script.println("<script>");
					script.println("alert('글쓰기에 실패했습니다.')");
					script.println("history.back()");
					script.println("</script>");
				}
				else {
					UserDAO userDAO = new UserDAO();
					userDAO.qpoint(id);			
					userDAO.changeGrade(id, userDAO.getInfo(id).getPoint());
					PrintWriter script=response.getWriter();
					script.println("<script>");
					script.println("location.href = 'main.jsp'");
					script.println("</script>");
				}
			}
		}
		}
	
		
		
		
		
	%>
</body>
</html>