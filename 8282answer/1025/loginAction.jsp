<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_member.UserDAO" %>
<%@ page import="java.io.PrintWriter" %>
<%@ page import="t_stop.StopDAO" %>
<%@ page import="bbs.BbsDAO" %>
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
		StopDAO stopDAO = new StopDAO();
		BbsDAO bbsDAO = new BbsDAO();
		int num_m= bbsDAO.idnum(user.getId());
		
		stopDAO.deleteStop_1();
		stopDAO.deleteStop_2();
		
		int result = userDAO.login(user.getId(), user.getPassword());
		if (result == 1 ){
			session.setAttribute("id",user.getId());
			PrintWriter script=response.getWriter();
		
			//누적 질문 신고 수에 따른 경고
			int report = userDAO.report_q(user.getId());
			
			if (report >= 5 && report <= 7) {
				script.println("<script>");
				script.println("alert('!!! 받은 질문 신고 내역이 5건 이상입니다 !!!')");
				script.println("</script>");
			} else if (report >= 8 && report <= 14) {
				script.println("<script>");
				script.println("alert('!!! 받은 질문 신고 내역이 8건 이상입니다 !!! 질문 작성 권한이 7일간 정지되었습니다.')");
				script.println("</script>");
				int a = stopDAO.confirm(num_m,0);
				if(a == -1){
				stopDAO.stop(bbsDAO.idnum(user.getId()), 0);
				}
			} else if (report >= 15) {
				script.println("<script>");
				script.println("alert('!!! 받은 질문 신고 내역이 15건 이상입니다 !!! 계정이 30일간 정지되었습니다.')");
				script.println("location.href='logoutAction.jsp'");
				script.println("</script>");
				int a = stopDAO.confirm(num_m,2);
				if (a == -1){
				stopDAO.stop(bbsDAO.idnum(user.getId()), 2);
				userDAO.updateReport_q(user.getId());
				}
			} 
			
			//누적 답변 신고 수에 따른 경고
			int report1 = userDAO.report_a(user.getId());
			
			if (report1 >=10 && report1 <=14) {
				script.println("<script>");
				script.println("alert('!!! 받은 답변 신고 내역이 10건 이상입니다 !!!')");
				script.println("</script>");
			} else if (report1 >= 15 && report1 <= 29) {
				script.println("<script>");
				script.println("alert('!!! 받은 답변 신고 내역이 15건 이상입니다 !!! 답변 작성 권한이 7일간 정지되었습니다.')");
				script.println("</script>");
				int a = stopDAO.confirm(num_m,1);
				if(a == -1){
				stopDAO.stop(bbsDAO.idnum(user.getId()), 1);
				}
			} else if (report1 >= 30) {
				script.println("<script>");
				script.println("alert('!!! 받은 답변 신고 내역이 30건 이상입니다 !!! 계정이 30일간 정지되었습니다.')");
				script.println("location.href='logoutAction.jsp'");
				script.println("</script>");
				int a = stopDAO.confirm(num_m,3);
				if(a == -1) {
				stopDAO.stop(bbsDAO.idnum(user.getId()), 3);
				userDAO.updateReport_a(user.getId());
				}
			} 
			
			
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